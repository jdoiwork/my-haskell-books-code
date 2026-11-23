-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | lam --check, and what an editor asks: read a file and check it
--   without running it, then tell the mistakes and the warnings with
--   their places, and the types under a place.
-- lam --check と、エディターが聞くこと：ファイルを読み、動かさずに
-- 確かめ、まちがいと警告を場所つきで知らせる。場所の下の型も
module Lam.Check
  ( Severity (..)
  , Problem (..)
  , Checked (..)
  , checkSource
  , problemOf
  , problemJson
  , wordAt
  , hoverAt
  , columnToIndex
  , indexToColumn
  ) where

-- ANCHOR: head
import Data.Aeson (Value, object, (.=))
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Text.Megaparsec.Unicode (charLength, stringLength)

import Lam.Cli (prepareCli)
import Lam.Module (Files, loadProgram, readProgram)
import Lam.Program (Session (..), checkMain, explain, placedType)
import Lam.Syntax (Decl (..), Expr, Loc (..), Program (..))
import Lam.Types (Type, inferType, renderScheme, renderType, typesAt)
-- ANCHOR_END: head

-- ANCHOR: problem
-- 🚩 A mistake stops the program; a warning only tells.
-- まちがいはプログラムを止める。警告は知らせるだけ
data Severity = AnError | Warning
  deriving (Eq, Show)

-- 📍 One thing to tell, at its place: the file, the line and the
--    column (from 1, as lam writes them), and the words. The text is
--    what lam writes for it, as it is.
-- 知らせること1つと、その場所：ファイル、行と桁（lam が書くとおり 1 から）、
-- 文。text は、lam がそれについて書く文そのまま
data Problem = Problem
  { problemFile     :: FilePath
  , problemLine     :: Int
  , problemColumn   :: Int
  , problemSeverity :: Severity
  , problemMessage  :: Text
  , problemText     :: Text
  }
  deriving (Eq, Show)

-- 📋 What checking a file gave: the problems, and the session when it
--    could be read (for the hover).
-- ファイルを確かめた答え：知らせることと、読めたならセッション（ホバーの
-- ため）
data Checked = Checked
  { checkedProblems :: [Problem]
  , checkedSession  :: Maybe Session
  }
-- ANCHOR_END: problem

-- ANCHOR: checkSource
-- ✅ Check a file without running it: read it (and its imports), then
--    check what lam would check before it starts: the command line of
--    main, the type of main, and the expression at the end. The
--    warnings come too. Nothing is done in the world.
-- ファイルを動かさずに確かめる：読み（import も）、lam が始める前に
-- 確かめることを確かめる。main のコマンドライン、main の型、最後の式。
-- 警告も付ける。世界には何もしない
checkSource
  :: Monad m => Files m -> Session -> FilePath -> Text -> m Checked
checkSource files base file source = do
  loaded <- loadProgram files base base file source
  pure $ case loaded of
    Left err -> Checked [problemOf file source err] Nothing
    Right (done, final) ->
      let warnings = map (problemOf file source)
                         (nub (sessionWarnings done))
          mistakes = either (\err -> [problemOf file source err])
                            (const []) (beforeStart done final)
      in Checked (mistakes <> warnings) (Just done)

-- 🚦 What lam checks before it starts the program.
-- lam がプログラムを始める前に確かめること
beforeStart :: Session -> Maybe Expr -> Either Text ()
beforeStart done final = do
  ready <- prepareCli done
  _ <- if Map.member "main" (sessionTypes ready)
         then () <$ checkMain ready
         else Right ()
  case final of
    Nothing -> Right ()
    Just e  -> case inferType (sessionData done) (sessionTypes done) e of
      Left err -> Left (joinPlace (placedType (explain done err)))
      Right _  -> Right ()
  where
    joinPlace (place, msg) = place <> msg
-- ANCHOR_END: checkSource

-- ANCHOR: problemOf
-- 🔎 Read the place back out of what lam writes. Most start with
--    file:line:column: (the type checker, the warnings, and megaparsec,
--    whose words come after the picture of the line). A mistake of a
--    whole definition (in the definition of x:, in main:) has no place:
--    then it is the line where the definition starts.
-- lam が書く文から、場所を読み戻す。たいていは ファイル:行:桁: で始まる
-- （型検査器、警告、megaparsec。megaparsec の文は、行の絵のあとに来る）。
-- 定義まるごとのまちがい（in the definition of x:、in main:）には場所が
-- ない。そのときは、定義が始まる行にする
problemOf :: FilePath -> Text -> Text -> Problem
problemOf file source text = case splitPlace text of
  Just (f, l, c, rest) -> made f l c rest
  Nothing ->
    let (l, c) = maybe (1, 1) (definitionLine source) (definedIn text)
    in made file l c text
  where
    made f l c rest =
      let body = cleaned rest
          severity
            | "warning: " `T.isPrefixOf` body = Warning
            | otherwise                       = AnError
      in Problem f l c severity (dropLabel body) text
    dropLabel b = case ( T.stripPrefix "warning: " b
                       , T.stripPrefix "error: " b ) of
      (Just w, _) -> w
      (_, Just e) -> e
      _           -> b

-- ✂️ file:line:column: and the rest.
-- ファイル:行:桁: と、残り
splitPlace :: Text -> Maybe (FilePath, Int, Int, Text)
splitPlace text = case T.splitOn ":" first of
  f : l : c : _
    | not (T.null f), isNumber l, isNumber c ->
        let place = T.intercalate ":" [f, l, c] <> ":"
        in Just ( T.unpack f, read (T.unpack l), read (T.unpack c)
                , T.drop (T.length place) text )
  _ -> Nothing
  where
    first = T.takeWhile (/= '\n') text
    isNumber t = not (T.null t) && T.all isDigit t

-- 🧹 The words only: megaparsec draws the line and a ^ under it
--    ("3 | let x = ", "  |     ^"); an editor draws its own line.
-- 文だけにする。megaparsec は行と、その下の ^ を描く。エディターは自分で
-- 線を引く
cleaned :: Text -> Text
cleaned rest = T.strip (T.intercalate "\n" (filter (not . picture)
                                             (T.lines rest)))
  where
    picture line =
      let (n, more) = T.span isDigit (T.stripStart line)
      in T.null (T.strip line)
           || ("|" `T.isPrefixOf` T.stripStart more && T.all isDigit n)
-- ANCHOR_END: problemOf

-- ANCHOR: definitionLine
-- 🏷️ The definition a mistake without a place is in.
-- 場所のないまちがいが起きた定義
definedIn :: Text -> Maybe Text
definedIn text
  | Just rest <- T.stripPrefix "in the definition of " text =
      Just (T.takeWhile (/= ':') rest)
  | "in main: " `T.isPrefixOf` text = Just "main"
  | otherwise = Nothing

-- 📏 The line where x is defined: a line that starts with x, then a
--    space, a : or a = (a definition starts at column 1). The first
--    line if there is none.
-- x を定義した行：x、それから空白か : か = で始まる行（定義は1桁目から
-- 始まる）。なければ1行目
definitionLine :: Text -> Text -> (Int, Int)
definitionLine source x =
  maybe (1, 1) (\n -> (n, 1)) $ listToMaybe
    [ n | (n, line) <- zip [1 ..] (T.lines source)
        , Just after <- [T.stripPrefix x line]
        , T.take 1 after `elem` ["", " ", ":", "="] ]
-- ANCHOR_END: definitionLine

-- ANCHOR: problemJson
-- 🧾 A problem for a machine: lam --check --json writes one a line.
-- 機械のための形：lam --check --json は1行に1つ書く
problemJson :: Problem -> Value
problemJson p = object
  [ "file"     .= problemFile p
  , "line"     .= problemLine p
  , "column"   .= problemColumn p
  , "severity" .= (case problemSeverity p of
                     AnError -> "error" :: Text
                     Warning -> "warning")
  , "message"  .= problemMessage p ]
-- ANCHOR_END: problemJson

-- ANCHOR: wordAt
-- 🔤 The word under a place (line and column from 1): a name (dots
--    too: String.trim), or an operator. Its column, and the word.
-- 場所（行と桁、1 から）の下の語：名前（点も：String.trim）か、演算子。
-- 語の桁と、語
wordAt :: Text -> Int -> Int -> Maybe (Int, Text)
wordAt source line column = do
  text <- listToMaybe (drop (line - 1) (T.lines source))
  let (before, after) = T.splitAt (columnToIndex text column) text
      kinds = [isNameChar, isSymbolChar]
  kind <- listToMaybe [ k | k <- kinds
                          , Just (ch, _) <- [T.uncons after], k ch ]
  let back = T.takeWhileEnd kind before
      word = back <> T.takeWhile kind after
      start = indexToColumn text (T.length before - T.length back)
  if T.null word then Nothing else Just (start, word)
  where
    isNameChar ch = isAlphaNum ch || ch `elem` ("_'." :: String)
    isSymbolChar ch = not (isAlphaNum ch || isSpace ch)
                        && ch `elem` ("+-*/<>=|&:!?%.$^~" :: String)

-- ANCHOR: columns
-- 📐 lam's columns are megaparsec's: a wide letter (日, 🧾) takes 2
--    columns, as on a terminal, and a letter that only changes the one
--    before it (the U+FE0F of 🗑️) takes none. These two turn a column
--    (from 1) into the number of letters before it, and back.
-- lam の桁は megaparsec の桁：幅の広い文字（日、🧾）は、端末と同じく
-- 2桁、前の文字を変えるだけの文字（🗑️ の U+FE0F）は0桁。この2つで、
-- 桁（1 から）を、その前にある文字の数にし、また戻す
columnToIndex :: Text -> Int -> Int
columnToIndex line column = go 0 0 (T.unpack line)
  where
    go i used rest = case rest of
      ch : more | used + charLength ch <= column - 1 ->
        go (i + 1) (used + charLength ch) more
      _ -> i

indexToColumn :: Text -> Int -> Int
indexToColumn line i = 1 + stringLength (T.unpack (T.take i line))
-- ANCHOR_END: columns
-- ANCHOR_END: wordAt

-- ANCHOR: hoverAt
-- 💬 What to show over a place: the type of the name there (a
--    definition, an import, a standard name), and the type of the call
--    or the operator that starts there. A name of a pattern or an
--    argument has no type here (see the weak points).
-- 場所の上に見せるもの：そこの名前の型（定義、import、標準の名前）と、
-- そこから始まる呼び出しか演算子の型。パターンや引数の名前の型は、ここ
-- にはない（弱いところを見る）
hoverAt :: Session -> FilePath -> Text -> Int -> Int -> Maybe Text
hoverAt session file source line column = do
  (start, word) <- wordAt source line column
  let named = [ word <> " : " <> renderScheme sc
              | not ("$" `T.isPrefixOf` word)
              , Just sc <- [Map.lookup word (sessionTypes session)] ]
      called = take 1 [ renderType t
                      | (Loc f l c, t) <- marksIn session file source
                      , f == file, l == line, c == start ]
      shown = [ "```lam\n" <> T.intercalate "\n" named <> "\n```"
              | not (null named) ]
           <> [ "the call from here : `" <> t <> "`" | t <- called ]
  if null shown then Nothing else Just (T.intercalate "\n\n" shown)

-- 📍 The types at the marks of the file: each definition and the
--    expression at the end, checked once more by typesAt.
-- ファイルの印ごとの型：定義と最後の式を、typesAt でもう一度確かめる
marksIn :: Session -> FilePath -> Text -> [(Loc, Type)]
marksIn session file source =
  case readProgram False (sessionOperators session) file source of
    Left _     -> []
    Right prog ->
      concatMap (typesAt (sessionData session) (sessionTypes session))
        ([ e | DefValue _ e <- programDecls prog ]
           <> maybe [] pure (programFinal prog))
-- ANCHOR_END: hoverAt
-- ANCHOR_END: all
