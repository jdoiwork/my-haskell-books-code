-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | One-liners, as in Perl: lam -e 'program', and -n, -p, -a, -F, -i,
--   --begin, --end, --json, --jsonl, --path, --as, -M. The flags only
--   write a Lam script around the program; lam runs that script as it
--   runs any file, and --print-script shows it.
-- Perl のようなワンライナー。フラグは、プログラムのまわりに Lam の
-- スクリプトを書くだけ。lam は、そのスクリプトをどのファイルとも同じく
-- 動かし、--print-script はそれを見せる
module Lam.OneLiner
  ( Mode (..)
  , Split (..)
  , Input (..)
  , Form (..)
  , OneLiner (..)
  , noOneLiner
  , Command (..)
  , readCommand
  , oneLinerUsage
  , Shape (..)
  , Part (..)
  , Script (..)
  , scriptOf
  , oneLinerLine
  , remap
  , prepare
  , runOneLiner
  , showValueOnly
  ) where

import Control.Monad (foldM)
import Data.Char (isAlpha, isAlphaNum)
import Data.List (find)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Megaparsec (errorBundlePretty)

import Lam.Eval (Request (..))
import Lam.Guard (Options, readOptions)
import Lam.Io (Concurrent, Handler)
import Lam.Module (Files, importAt, loadProgram, runProgram, runProgramWith)
import Lam.Parser (importLine, parseLam, runLamParser, spaces)
import Lam.Pcre (compileRegex)
import Lam.Program
import Lam.Syntax
import Lam.Types
import Lam.Utf8 (Decoding (..))
-- ANCHOR_END: head

-- ANCHOR: types
-- 🚩 What to do with the program: run it once (-e), once for each
--    line (-n), or once for each line and print what it gives (-p).
--    -p is stronger than -n, as in Perl.
-- プログラムをどう動かすか：1回（-e）、1行ごと（-n）、1行ごとに動かして
-- 答えを出す（-p）。Perl と同じく、-p は -n より強い
data Mode = Once | EachLine | Printing
  deriving (Eq, Ord, Show)

-- ✂️ How -a cuts a line into columns: at spaces (as awk does), or at a
--    regular expression (-F, or -F: or -F'\t').
-- -a が行を列に切るやり方：空白で（awk と同じ）、または正規表現で（-F）
data Split = NoSplit | ByWords | ByRegex Text
  deriving (Eq, Show)

-- 📥 What one piece of the input is: a line of text, or JSON. --json
--    reads the whole input as one JSON, --jsonl one JSON on each line
--    (JSON Lines).
-- 入力の1つ分：テキストの1行か、JSON か。--json は入力全体を1つの
-- JSON として、--jsonl は1行に1つの JSON（JSON Lines）として読む
data Input = TextLines | WholeJson | JsonLines
  deriving (Eq, Show)

-- 📜 The main of the script: with options as in the chapter on the
--    command line (lam it.lam --help works), or plain.
-- スクリプトの main：コマンドラインの章のオプションつきか、素朴な形か
data Form = CliForm | PlainForm
  deriving (Eq, Show)

-- 📋 Everything the one-liner flags said.
-- ワンライナーのフラグが言ったこと全部
data OneLiner = OneLiner
  { oneProgram  :: [Text]       -- each -e, in order
  , oneMode     :: Mode
  , oneSplit    :: Split
  , oneInPlace  :: Maybe Text   -- -i: Just "" (no backup) or Just ".bak"
  , oneBegin    :: Maybe Text   -- --begin: the first acc
  , oneEnd      :: Maybe Text   -- --end: what to do with the last acc
  , oneInput    :: Input        -- --json, --jsonl
  , onePath     :: [Text]       -- --path=orders.items: ["orders", "items"]
  , oneAs       :: Maybe Text   -- --as=Order: read each piece as an Order
  , oneDecoding :: Maybe Decoding  -- --decoding=strict: broken bytes
  , oneModules  :: [Text]       -- -MPrice: import Price (..)
  , oneScript   :: Maybe Form   -- --print-script: show it, run nothing
  }
  deriving (Eq, Show)

noOneLiner :: OneLiner
noOneLiner = OneLiner [] Once NoSplit Nothing Nothing Nothing TextLines
  [] Nothing Nothing [] Nothing

-- ANCHOR: decodingOf
-- 🧹 How broken bytes are read, when --decoding does not say: skipped
--    for the screen, but a stop for -i. Lam reads text, not bytes, so
--    a skipped byte would leave the file it writes back, quietly.
-- --decoding がなければ、壊れたバイトの読み方は：画面に出すだけなら
-- 飛ばす。-i なら止まる。Lam はバイトではなく文字として読むので、
-- 飛ばしたバイトは、書き戻すファイルから黙って消えてしまう
decodingOf :: OneLiner -> Decoding
decodingOf one = case (oneDecoding one, oneInPlace one) of
  (Just how, _)      -> how
  (Nothing, Just _)  -> Strict
  (Nothing, Nothing) -> Ignore
-- ANCHOR_END: decodingOf

-- 🧭 What lam was asked to do.
-- lam が頼まれたこと
data Command
  = Repl
  | RunFile FilePath [String]       -- lam file.lam args...
  | RunOneLiner OneLiner [String]   -- lam -e '...' files or args...
  deriving (Eq, Show)
-- ANCHOR_END: types

-- ANCHOR: readCommand
-- 🚩 Read the flags before the file (or before the input files of a
--    one-liner). Short flags go together, as in Perl:  -ane '...'  is
--    -a -n -e '...'. -e, -F, -M and -i take the rest of their word
--    (-F,  -i.bak  -MPrice); -e, -F and -M take the next word when
--    nothing is left. The other long flags (--allow-net, --trace, ...)
--    go to readOptions. The first word that is not a flag ends them.
-- ファイル（ワンライナーなら入力のファイル）より前のフラグを読む。
-- 短いフラグは、Perl と同じくまとめて書ける。-e・-F・-M・-i は、その語の
-- 残りを取る。-e・-F・-M は、残りがなければ次の語を取る。ほかの長い
-- フラグは readOptions に渡す。フラグでない最初の語で終わる
readCommand :: [String] -> Maybe String -> Either Text (Options, Command)
readCommand args0 fromEnv = go [] noOneLiner args0
  where
    go guards one args = case args of
      -- After --, every word is a file (trailing drops the --).
      -- -- の後ろは、どの語もファイル（-- は trailing が外す）
      "--" : _ -> finish guards one args
      -- After a program, only another -e, --begin and --end: any other
      -- word is a file to read, or an argument of the program.
      -- プログラムの後ろには、-e・--begin・--end だけ。ほかの語は、読む
      -- ファイルか、プログラムの引数
      w : _ | not (null (oneProgram one))
            , w `notElem` ["-e", "--begin", "--end"] ->
          finish guards one args
      "--begin" : x : rest -> go guards one { oneBegin = Just (T.pack x) }
                                rest
      "--end"   : x : rest -> go guards one { oneEnd = Just (T.pack x) } rest
      [flag] | flag `elem` ["--begin", "--end"] ->
        Left ("lam: " <> T.pack flag <> " needs a Lam expression after it")
      "--json"  : rest -> reading WholeJson rest
      "--jsonl" : rest -> reading JsonLines rest
      flag@('-' : '-' : _) : rest -> case longFlag one (T.pack flag) of
        Just changed -> changed >>= \one' -> go guards one' rest
        Nothing      -> go (guards <> [flag]) one rest
      ('-' : c : cs) : rest | isAlpha c -> short guards one (c : cs) rest
      _ -> finish guards one args
      where
        reading input rest
          | oneInput one /= TextLines && oneInput one /= input =
              Left "lam: --json and --jsonl cannot go together"
          | otherwise =
              go guards (atLeast EachLine one) { oneInput = input } rest

    -- One word of short flags, one letter at a time.
    -- 短いフラグの1語を、1文字ずつ
    short guards one letters rest = case letters of
      [] -> go guards one rest
      'n' : more -> short guards (atLeast EachLine one) more rest
      'p' : more -> short guards (atLeast Printing one) more rest
      'a' : more -> short guards (splitting ByWords one) more rest
      'F' : more -> taking 'F' more rest $ \pat rest' ->
        go guards (splitting (ByRegex (T.pack pat)) one) rest'
      'e' : more -> taking 'e' more rest $ \prog rest' ->
        go guards one { oneProgram = oneProgram one <> [T.pack prog] } rest'
      'M' : more -> taking 'M' more rest $ \m rest' ->
        go guards one { oneModules = oneModules one <> [T.pack m] } rest'
      'i' : more -> go guards one { oneInPlace = Just (T.pack more) } rest
      c : _ -> Left ("lam: unknown flag: -" <> T.singleton c)

    -- The rest of the word, or (when it is empty) the next word.
    -- 語の残り。空なら次の語
    taking c more rest k = case (more, rest) of
      ("", next : rest') -> k next rest'
      ("", [])           -> Left ("lam: -" <> T.singleton c
                                    <> " needs something after it")
      _                  -> k more rest

    atLeast m one = one { oneMode = max m (oneMode one) }
    -- -a and -F mean -n, as in Perl (5.20 and later).
    -- Perl（5.20 から）と同じく、-a と -F は -n も意味する
    splitting s one = atLeast EachLine one
      { oneSplit = case (s, oneSplit one) of
          (ByWords, ByRegex p) -> ByRegex p
          _                    -> s }

    finish guards one rest0 = do
      (opts, _) <- readOptions guards fromEnv
      (one', rest) <- trailing one rest0
      command <- if one == noOneLiner
        then Right (case rest of
                      []          -> Repl
                      file : more -> RunFile file more)
        else RunOneLiner <$> checked one' rest <*> Right rest
      Right (opts, command)
-- ANCHOR_END: readCommand

-- ANCHOR: trailing
-- 📜 The words after the program of -n or -p are files, but
--    --print-script may come there too: it only shows the script, so it
--    is lam's wherever it is (lam -ne '...' --print-script | tee a.lam).
--    Another flag of lam there is told: it goes before -e. After --,
--    every word is a file. With -e alone (a whole program, like a file),
--    the words are the program's.
-- -n・-p のプログラムの後ろの語はファイル。でも --print-script は後ろにも
-- 書ける。スクリプトを見せるだけなので、どこにあっても lam のもの。ほかの
-- lam のフラグが後ろにあれば、-e より前に、と言う。-- の後ろは全部ファイル。
-- -e だけ（ファイルと同じく、プログラム全体）なら、語はプログラムのもの
trailing :: OneLiner -> [String] -> Either Text (OneLiner, [String])
trailing one given
  | one == noOneLiner = Right (one, unDashed)
  | oneMode one == Once, oneBegin one == Nothing, oneEnd one == Nothing =
      Right (one, unDashed)
  | otherwise = walk one given
  where
    walk o ws = case ws of
      [] -> Right (o, [])
      "--" : rest -> Right (o, rest)
      w : rest
        | isPrintScript w -> case longFlag o (T.pack w) of
            Just changed -> changed >>= \o' -> walk o' rest
            Nothing      -> walk o rest
        | lamFlag w -> Left (lateFlag w)
        | otherwise -> do
            (o', more) <- walk o rest
            Right (o', w : more)
    unDashed = case given of
      "--" : rest -> rest
      _           -> given
    isPrintScript w =
      w == "--print-script" || "--print-script=" `T.isPrefixOf` T.pack w
    -- A flag lam knows (--dry-run, --allow-read=data, --json, ...).
    -- lam の知っているフラグ
    lamFlag w = case w of
      '-' : '-' : _ : _ ->
        either (const False) (null . snd) (readOptions [w] Nothing)
          || w `elem` ["--json", "--jsonl", "--begin", "--end"]
          || isFlagOfOneLiner w
      _ -> False
    isFlagOfOneLiner w = case longFlag noOneLiner (T.pack w) of
      Just _  -> True
      Nothing -> False
    lateFlag w = "lam: " <> T.pack w <> " goes before -e (after the \
      \program come the files)"
-- ANCHOR_END: trailing

-- ANCHOR: longFlag
-- 🚩 The long flags of one-liners with a value. Nothing: not one of
--    them (it goes to readOptions).
-- 値のある、ワンライナーの長いフラグ。Nothing なら、そうではない
longFlag :: OneLiner -> Text -> Maybe (Either Text OneLiner)
longFlag one flag
  | flag == "--print-script" = Just (Right one { oneScript = Just CliForm })
  | Just form <- T.stripPrefix "--print-script=" flag = Just $ case form of
      "cli"   -> Right one { oneScript = Just CliForm }
      "plain" -> Right one { oneScript = Just PlainForm }
      _       -> Left "lam: --print-script=cli or --print-script=plain"
  | Just keys <- T.stripPrefix "--path=" flag = Just $
      if T.null keys || any T.null (T.splitOn "." keys)
        then Left "lam: --path needs keys: --path=orders.items"
        else Right one { onePath = T.splitOn "." keys }
  | Just ty <- T.stripPrefix "--as=" flag = Just $
      if T.null (T.strip ty) then Left "lam: --as needs a type: --as=Order"
      else Right one { oneAs = Just ty }
  | Just how <- T.stripPrefix "--decoding=" flag = Just $ case how of
      "strict"  -> Right one { oneDecoding = Just Strict }
      "replace" -> Right one { oneDecoding = Just Replace }
      "ignore"  -> Right one { oneDecoding = Just Ignore }
      _ -> Left "lam: --decoding=strict, --decoding=replace or \
                \--decoding=ignore"
  | otherwise = Nothing
-- ANCHOR_END: longFlag

-- 📖 How to use the one-liners, after the usage of the flags.
-- ワンライナーの使い方。フラグの使い方のあとに見せる
oneLinerUsage :: Text
oneLinerUsage = T.unlines
  [ "       lam [flags] [-n|-p] [-a] [-F regex] [-i[.bak]] [-M module]"
  , "           [--begin expr] [--end expr] [--json|--jsonl] [--path=keys]"
  , "           [--as=Type] [--decoding=strict|replace|ignore]"
  , "           [--print-script[=cli|plain]] -e 'program' [files...]" ]

-- ANCHOR: checked
-- ✅ Flags that cannot go together are an error before anything runs.
-- 一緒に使えないフラグは、何かを動かす前にエラーにする
checked :: OneLiner -> [String] -> Either Text OneLiner
checked one rest
  | null (oneProgram one) =
      Left "lam: -n, -p, -a, -F, -i, -M, --begin and --end need -e 'program'"
  | Just _ <- oneScript one, oneMode one == Once, not foldingAny =
      Left "lam: --print-script goes with -n or -p (with -e alone, the \
           \program is a script already)"
  | Just _ <- oneInPlace one, oneMode one /= Printing =
      Left "lam: -i needs -p (each line becomes what the program gives)"
  | Just _ <- oneInPlace one, null rest, oneScript one == Nothing =
      Left "lam: -i needs the files to change"
  | oneMode one == Printing, foldingAny =
      Left "lam: --begin and --end go with -n, not with -p"
  | Just _ <- oneEnd one, Nothing <- oneBegin one =
      Left "lam: --end needs --begin (the first acc)"
  | ByRegex p <- oneSplit one, Left msg <- compileRegex p =
      Left ("lam: -F: " <> msg)
  -- ANCHOR: jsonChecks
  | oneInput one == TextLines, not (null (onePath one)) =
      Left "lam: --path goes with --json or --jsonl"
  | oneInput one == TextLines, Just _ <- oneAs one =
      Left "lam: --as goes with --json or --jsonl"
  | oneInput one /= TextLines, oneSplit one /= NoSplit =
      Left "lam: -a and -F go with text, not with --json or --jsonl"
  | Just _ <- oneInPlace one
  , oneInput one == WholeJson || not (null (onePath one)) =
      Left "lam: -i goes with text or --jsonl, without --path"
  -- ANCHOR_END: jsonChecks
  | foldingAny = Right one { oneMode = max EachLine (oneMode one) }
  | otherwise  = Right one
  where
    foldingAny = oneBegin one /= Nothing || oneEnd one /= Nothing
-- ANCHOR_END: checked

-- ANCHOR: shape
-- 📐 What the types decided: does the program still wait for its
--    piece after its names (s/cat/dog/g), does --end still wait for
--    the acc (print), and does -p give a Json.
-- 型が決めたこと：名前をもらっても、プログラムがまだ1つ分を待っているか
-- （s/cat/dog/g）、--end がまだ acc を待っているか（print）、-p が Json
-- を返すか
data Shape = Shape
  { takesPiece :: Bool
  , takesAcc   :: Bool
  , givesJson  :: Bool
  }
  deriving (Eq, Show)

-- 🧷 Where a piece the person wrote is in the script: its name (-e,
--    --begin, --end), its first line, how far it is pushed right, and
--    how many lines it has. An error there is told at the person's
--    own place:  -e:1:16, not <one-liner>:12:18.
-- 人が書いたかけらが、スクリプトのどこにあるか：名前、最初の行、右へ
-- ずらした桁、行の数。そこのエラーは、人が書いた場所で言う
data Part = Part
  { partName   :: Text
  , partLine   :: Int
  , partIndent :: Int
  , partLines  :: Int
  }
  deriving (Eq, Show)

-- 📜 A script: its text, and where the pieces of the person are.
-- スクリプト：その文字と、人が書いたかけらの場所
data Script = Script
  { scriptText  :: Text
  , scriptParts :: [Part]
  }
  deriving (Eq, Show)
-- ANCHOR_END: shape

-- ANCHOR: chunk
-- 🧱 A script is written in chunks: lines of our own, and the pieces
--    the person wrote (kept apart, to know where they are).
-- スクリプトは、かたまりで書く：こちらの行と、人が書いたかけら（場所を
-- 知るため、分けておく）
data Chunk
  = Lines [Text]
  | Piece Text Text [Text] Text   -- the name, before, the lines, after

-- Put the chunks together, and count the lines of the pieces.
-- かたまりをつなぎ、かけらの行を数える
assemble :: [Chunk] -> Script
assemble chunks = Script (T.unlines (concat texts)) (concat parts)
  where
    (texts, parts) = unzip (go 1 chunks)
    go _ [] = []
    go n (c : cs) = case c of
      Lines ls -> (ls, []) : go (n + length ls) cs
      -- before goes in front of the first line (and spaces as wide in
      -- front of the others), after behind the last one.
      -- before は最初の行の前に（ほかの行の前には同じ幅の空白）、after は
      -- 最後の行の後ろに
      Piece name before ls after ->
        let indent = T.length before
            fronts = before : repeat (T.replicate indent " ")
            pieceLines = zipWith (<>) fronts ls
            closed = case reverse pieceLines of
              lastLine : others -> reverse ((lastLine <> after) : others)
              []                -> []
        in (closed, [Part name n indent (length ls)])
             : go (n + length ls) cs
-- ANCHOR_END: chunk

-- ANCHOR: scriptOf
-- 📝 The script of a one-liner, as the shape says.
-- ワンライナーのスクリプト。形は shape のとおり
scriptOf :: OneLiner -> Shape -> Script
scriptOf one shape = assemble $
  header one
  <> imports one
  <> helpers one
  <> stepChunks one shape
  <> foldChunks one shape
  <> visitChunks one shape
  <> mainChunks one
-- ANCHOR_END: scriptOf

-- ANCHOR: header
-- 🏷️ What the script is, and the one-liner it came from.
-- スクリプトが何か、どのワンライナーから作ったか
header :: OneLiner -> [Chunk]
header one =
  [ Lines (
    [ "#!/usr/bin/env lam"
    , "-- 📜 Made with lam --print-script: the same as this one-liner:"
    , "-- --print-script で作った、このワンライナーと同じ動きのスクリプト：" ]
    <> map ("--   " <>) (T.lines (oneLinerLine one)) <>
    [ "-- Run it:  lam this.lam [files...]   (no files: the standard input)"
    , "-- 動かし方：lam このファイル.lam [ファイル…]（なければ標準入力）"
    , "" ]) ]

-- 🐚 The one-liner as a line of sh, each word quoted when it needs it
--    (a program with line breaks gives more lines).
-- ワンライナーを sh の1行に。要る語だけを引用符で囲む（改行のある
-- プログラムなら、何行かになる）
oneLinerLine :: OneLiner -> Text
oneLinerLine one = T.unwords ("lam" : map shellWord flags)
  where
    flags = concat
      [ [ "-M" <> m | m <- oneModules one ]
      , [ "-F" <> p | ByRegex p <- [oneSplit one] ]
      , [ "-a" | oneSplit one == ByWords ]
      , case oneInput one of
          TextLines -> []
          WholeJson -> ["--json"]
          JsonLines -> ["--jsonl"]
      , [ "--path=" <> T.intercalate "." (onePath one)
        | not (null (onePath one)) ]
      , [ "--as=" <> t | Just t <- [oneAs one] ]
      , [ "--decoding=" <> decodingName how | Just how <- [oneDecoding one] ]
      , [ "-i" <> ext | Just ext <- [oneInPlace one] ]
      , [ if oneMode one == Printing then "-p" else "-n" ]
      , concat [ ["--begin", b] | Just b <- [oneBegin one] ]
      , concatMap (\e -> ["-e", e]) (oneProgram one)
      , concat [ ["--end", e] | Just e <- [oneEnd one] ] ]

shellWord :: Text -> Text
shellWord w
  | not (T.null w), T.all plain w = w
  | otherwise = "'" <> T.replace "'" "'\\''" w <> "'"
  where
    plain c = isAlphaNum c || c `elem` ("-_./=:,@+%" :: String)

decodingName :: Decoding -> Text
decodingName how = case how of
  Strict  -> "strict"
  Replace -> "replace"
  Ignore  -> "ignore"

-- The Lam name of the Decoding.
-- Decoding の Lam の名前
decodingCon :: Decoding -> Text
decodingCon = T.pack . show

-- 📦 -MPrice is  import Price (..),  and -M'Price as P'  is as it is.
-- -MPrice は import Price (..)。-M'Price as P' は、そのまま
importText :: Text -> Text
importText m
  | T.any (== ' ') (T.strip m) || T.any (== '(') m = "import " <> T.strip m
  | otherwise = "import " <> m <> " (..)"

imports :: OneLiner -> [Chunk]
imports one = case oneModules one of
  [] -> []
  ms -> [Lines (map importText ms <> [""])]
-- ANCHOR_END: header

-- ANCHOR: helpers
-- 🔧 The helpers the flags need: field for -a, pick for --path, and
--    the rest for JSON and -i.
-- フラグが要る道具：-a の field、--path の pick、JSON と -i のための残り
helpers :: OneLiner -> [Chunk]
helpers one = concat
  [ [ fieldHelper | oneSplit one /= NoSplit ]
  , [ stopHelper | oneInput one /= TextLines || oneInPlace one /= Nothing ]
  , [ pickHelper | not (null (onePath one)) ]
  , [ asHelper t | Just t <- [oneAs one] ]
  , [ nameHelper | oneInput one /= TextLines ]
  , [ piecesHelper one | oneInput one /= TextLines ] ]

nameHelper :: Chunk
nameHelper = Lines
  [ "-- 🏷️ The name of a file in a message (\"-\" is the standard input)."
  , "-- 知らせの文の中の、ファイルの名前（\"-\" は標準入力）"
  , "nameOf file = if file == \"-\" then \"<stdin>\" else file"
  , "" ]

-- ANCHOR: fieldHelper
fieldHelper :: Chunk
fieldHelper = Lines
  [ "-- 🔢 field n is the column n: 1 is the first, 0 the whole line,"
  , "--    -1 the last (awk's $NF), and a column that is not there is \"\"."
  , "-- field n は n 列目。1 が1列目、0 は行全体、-1 は最後（awk の $NF）、"
  , "-- ない列は \"\""
  , "fieldOf : String -> [String] -> Int -> String"
  , "fieldOf line fields n ="
  , "  if n == 0 then line"
  , "  else if n < 0 then"
  , "    (if length fields + n < 0 then \"\""
  , "     else fieldOf line fields (length fields + 1 + n))"
  , "  else case drop (n - 1) fields of"
  , "    | x : _ -> x"
  , "    | [] -> \"\""
  , "" ]

-- ANCHOR_END: fieldHelper
-- ANCHOR: jsonHelpers
stopHelper :: Chunk
stopHelper = Lines
  [ "-- 🛑 An Err stops the script with its message (exit code 1)."
  , "-- Err なら、わけを言ってスクリプトを止める（終了コード 1）"
  , "orStop : Result String a -> IO a"
  , "orStop result ="
  , "  case result of"
  , "    | Ok x -> pure x"
  , "    | Err why -> do"
  , "        ePutStrLn f\"error: {why}\""
  , "        exit 1"
  , "" ]

pickHelper :: Chunk
pickHelper = Lines
  [ "-- 🧭 Walk down by keys (a number is the place in an array, from 0)."
  , "-- キーをたどって下りる（数なら、配列の何番目か。0 から）"
  , "down : String -> Json -> Maybe Json"
  , "down key j ="
  , "  case j of"
  , "    | JObj m -> Map.lookup key m"
  , "    | JArr xs ->"
  , "        case (String.toInt key, drop (String.toInt key ?: 0) xs) of"
  , "          | (Just i, x : _) -> if i < 0 then Nothing else Just x"
  , "          | _ -> Nothing"
  , "    | _ -> Nothing"
  , ""
  , "pick : [String] -> Json -> Result String Json"
  , "pick keys j ="
  , "  case keys of"
  , "    | [] -> Ok j"
  , "    | key : rest ->"
  , "        case down key j of"
  , "          | Just x -> pick rest x"
  , "          | Nothing -> Err key"
  , "" ]

-- ANCHOR_END: jsonHelpers
-- ANCHOR: asHelper
asHelper :: Text -> Chunk
asHelper t = Lines
  [ "-- 🎯 --as=" <> t <> ": each piece is read as " <> article t
  , "--    (with the decoder of its instance Decodable)."
  , "-- 1つ分ずつを " <> t <> " として読む（Decodable の Decoder で）"
  , "decodeAs : String -> Json -> Result String " <> typeText t
  , "decodeAs place j ="
  , "  Decode.run Decode.decoder j"
  , "    |> Result.mapError (\\ps -> f\"{place}:\\n{Decode.explain ps}\")"
  , "" ]
  where
    article ty = case T.uncons ty of
      Just (c, _) | c `elem` ("AEIOU" :: String) -> "an " <> ty
      _                                          -> "a " <> ty

-- ANCHOR_END: asHelper
-- ANCHOR: piecesHelper
-- A type in an annotation: in parentheses when it has spaces.
-- 注釈の中の型。空白があれば、かっこに入れる
typeText :: Text -> Text
typeText t
  | T.any (== ' ') (T.strip t) = "(" <> T.strip t <> ")"
  | otherwise                  = T.strip t

-- 🧩 The JSON in a text (a line of --jsonl, or all of --json): at
--    --path, and an array gives its items. A line with only spaces
--    has none.
-- 文字列の中の JSON（--jsonl の1行か、--json の全体）。--path の所の
-- もので、配列なら要素の1つずつ。空白だけの行には、何もない
piecesHelper :: OneLiner -> Chunk
piecesHelper one = Lines $
  [ "-- 🧩 The JSON in a text" <> at <> "." <> arrays
  , "-- 文字列の中の JSON" <> atJa <> "。" <> arraysJa
  , "piecesOf : String -> String -> Result String [" <> pieceType <> "]"
  , "piecesOf place text ="
  , "  if String.trim text == \"\" then Ok []"
  , "  else do"
  , "    j <- Json.parse text"
  , "           |> Result.mapError (\\e -> f\"{place}: not JSON: {e}\")" ]
  <> walk
  <> [ "    " <> final
     , "" ]
  where
    keys = onePath one
    dotted = T.intercalate "." keys
    at = if null keys then "" else ", at --path=" <> dotted
    atJa = if null keys then "" else " の、--path=" <> dotted <> " の所"
    spread = oneInput one == WholeJson || not (null keys)
    arrays = if spread then " An array gives its items." else ""
    arraysJa = if spread then "配列なら、要素の1つずつ" else "1行に1つ"
    walk
      | null keys = []
      | otherwise =
          [ "    v <- pick " <> listOf keys <> " j"
          , "           |> Result.mapError (\\k -> f\"{place}: --path="
              <> dotted <> ": no {k}\")" ]
    found = if null keys then "j" else "v"
    items
      | spread = "(case " <> found <> " of | JArr xs -> xs | _ -> ["
                   <> found <> "])"
      | otherwise = "[" <> found <> "]"
    final = case oneAs one of
      Nothing -> "pure " <> items
      Just _  -> "mapM (decodeAs place) " <> items
    pieceType = maybe "Json" typeText (oneAs one)
    listOf ks = "[" <> T.intercalate ", " (map quoted ks) <> "]"
    quoted k = "\"" <> k <> "\""
-- ANCHOR_END: piecesHelper
-- ANCHOR_END: helpers

-- ANCHOR: stepChunks
-- 🔁 The program of -e. It gets line (or json), and with --begin the
--    acc first; with -a, fields and field, too. When it is still a
--    function after them, it gets line, too:  s/cat/dog/g line.
-- -e のプログラム。line（か json）をもらう。--begin があれば、その前に
-- acc も。-a なら fields と field も。それでもまだ関数なら、line も渡す
stepChunks :: OneLiner -> Shape -> [Chunk]
stepChunks one shape =
  [ Lines (
    [ "-- 🔁 The program of -e, for one " <> piece <> "." ]
    <> stillNote <>
    [ "-- -e のプログラム。" <> piece <> " を1つ受け取る" <> stillNoteJa
    , "step " <> accArg <> piece <> " =" ]
    <> splitLines) ]
  <> body "-e" (T.intercalate "\n" (oneProgram one)) (takesPiece shape)
       piece
  <> [Lines [""]]
  where
    piece = pieceName one
    accArg = if folding one then "acc " else ""
    stillNote =
      [ "--    It is still a function, so it gets " <> piece <> ", too."
      | takesPiece shape ]
    stillNoteJa = if takesPiece shape
      then "。まだ関数なので、" <> piece <> " も渡す" else ""
    splitLines = case oneSplit one of
      NoSplit -> []
      split ->
        [ "  let fields = " <> cut split <> " in"
        , "  let field = fieldOf line fields in" ]
    cut split = case split of
      ByRegex p -> "Regex.split re\"" <> T.replace "\"" "\\\"" p <> "\" line"
      _         -> "words line"

-- 🧩 A piece the person wrote, as the body of a definition: as it is
--    (2 to the right), or in parentheses with an argument after it.
-- 人が書いたかけらを、定義の中身に：そのまま（2桁右へ）か、かっこに
-- 入れて、後ろに引数を付けるか
body :: Text -> Text -> Bool -> Text -> [Chunk]
body name text applied arg = case (applied, T.lines text) of
  (False, ls)  -> [Piece name "  " ls ""]
  (True, [l])  -> [Piece name "  (" [l] (") " <> arg)]
  (True, ls)   ->
    [Lines ["  ("], Piece name "    " ls "", Lines ["  ) " <> arg]]

pieceName :: OneLiner -> Text
pieceName one = case oneInput one of
  TextLines -> "line"
  _         -> "json"

folding :: OneLiner -> Bool
folding one = oneBegin one /= Nothing
-- ANCHOR_END: stepChunks

-- ANCHOR: foldChunks
-- 🌱 --begin and --end: the first acc, and what to do with the last.
-- --begin と --end：初めの acc と、最後の acc ですること
foldChunks :: OneLiner -> Shape -> [Chunk]
foldChunks one shape = concat
  [ case oneBegin one of
      Nothing -> []
      Just b ->
        [ Lines [ "-- 🌱 --begin: the first acc. / 初めの acc", "start =" ] ]
        <> body "--begin" b False "" <> [Lines [""]]
  , case oneEnd one of
      Nothing -> []
      Just e ->
        [ Lines [ "-- 🏁 --end: what to do with the last acc."
                , "-- 最後の acc ですること", "finish acc =" ] ]
        <> body "--end" e (takesAcc shape) "acc" <> [Lines [""]] ]
-- ANCHOR_END: foldChunks

-- ANCHOR: visitChunks
-- 👣 One line, with its number: what -n, -p, --begin and -i do with
--    it. For JSON, a line has pieces, and each one is done.
-- 1行（と、その番号）でやること。JSON なら、1行の中のかけらを1つずつ
visitChunks :: OneLiner -> Shape -> [Chunk]
visitChunks one shape = case oneInput one of
  TextLines -> [ Lines (
    [ "-- 👣 One line, with its number: give the acc for the next one."
    , "-- 1行（と番号）をやって、次の行のための acc を返す"
    ] <> defining "visit acc n line" (doing "line") <> [""]) ]
  _ -> [ Lines (
    [ "-- 👣 One piece: give the acc for the next one."
    , "-- 1つ分をやって、次の1つ分のための acc を返す"
    ] <> defining "each acc json" (doing "json") <>
    [ ""
    , "eachOf acc pieces ="
    , "  case pieces of"
    , "    | [] -> pure acc"
    , "    | piece : rest -> do"
    , "        next <- each acc piece"
    , "        eachOf next rest"
    , "" ] <> visitJson) ]
  where
    -- What one piece does.
    -- 1つ分でやること
    doing x
      | folding one = ["pure (step acc " <> x <> ")"]
      | Just _ <- oneInPlace one =
          [ "do"
          , "  written <- File.append acc (" <> shown x <> " ++ \"\\n\")"
          , "  orStop written"
          , "  pure acc" ]
      | oneMode one == Printing =
          [ "do", "  putStrLn (" <> shown x <> ")", "  pure acc" ]
      | otherwise = [ "do", "  step " <> x, "  pure acc" ]
    shown x
      | givesJson shape = "Json.show (step " <> x <> ")"
      | otherwise       = "step " <> x
    visitJson = case oneInput one of
      WholeJson ->
        [ "-- 📥 --json: all of a file is one JSON (\"-\": the standard input)."
        , "-- --json：ファイル全体で1つの JSON（\"-\" は標準入力）"
        , "readOne acc file = do"
        , "  text <- (if file == \"-\" then map Ok getContents"
        , "           else readFile file) ?"
        , "  pieces <- pure (piecesOf (nameOf file) text) ?"
        , "  map Ok (eachOf acc pieces)"
        , "" ]
      _ ->
        [ "-- 👣 One line of JSON Lines, with its number."
        , "-- JSON Lines の1行（と番号）"
        , "visit file acc n line = do"
        , "  pieces <- orStop (piecesOf f\"{nameOf file}:{n}\" line)"
        , "  eachOf acc pieces"
        , "" ]

-- name = first line, and the rest below it:  visit acc n line = do
-- name = 最初の行。残りはその下に
defining :: Text -> [Text] -> [Text]
defining name ls = case ls of
  first : rest -> (name <> " = " <> first) : rest
  []           -> [name <> " ="]
-- ANCHOR_END: visitChunks

-- ANCHOR: mainChunks
-- 🚪 The files one after another ("-", the standard input, when there
--    are none), and main.
-- ファイルを順に（なければ "-"、標準入力）。そして main
mainChunks :: OneLiner -> [Chunk]
mainChunks one = [ Lines (readAll <> mainLines) ]
  where
    how = decodingCon (decodingOf one)
    visiting = case oneInput one of
      TextLines -> "visit"
      _         -> "(visit file)"
    -- One file:  File.forLines Ignore file acc visit  (or readOne).
    -- ファイル1つ
    readFile1 = case oneInput one of
      WholeJson -> "readOne acc file ?"
      _ -> "File.forLines " <> how <> " file acc " <> visiting <> " ?"
    readAll = case oneInPlace one of
      Nothing ->
        [ "-- 📂 The files, one after another (\"-\" is the standard input)."
        , "-- ファイルを、順に（\"-\" は標準入力）"
        , "readAll acc files ="
        , "  case files of"
        , "    | [] -> pure (Ok acc)"
        , "    | file : rest -> do"
        , "        next <- " <> readFile1
        , "        readAll next rest"
        , "" ]
      -- ANCHOR: editLines
      Just ext ->
        [ "-- ✏️ -i: one file changed in place. The new lines go to a file"
        , "--    next to it, which takes the file's name at the end. If it"
        , "--    stops before that, defer removes it: the file stays."
        , "-- -i：ファイルをその場で書き換える。新しい行は隣のファイルに"
        , "-- 書き、最後にファイルの名前を取らせる。その前に止まったら、"
        , "-- defer がそれを消すので、ファイルは元のまま"
        , "edit file = do"
        , "  let tmp = file ++ \".lam-tmp\""
        , "  defer File.remove tmp"
        , "  writeFile tmp \"\" ?"
        , "  File.forLines " <> how <> " file tmp " <> visiting <> " ?" ]
        <> [ "  File.copy file (file ++ \"" <> ext <> "\") ?"
           | not (T.null ext) ] <>
        [ "  File.rename tmp file ?"
        , "  pure (Ok ())"
        , ""
        , "editAll files ="
        , "  case files of"
        , "    | [] -> pure (Ok ())"
        , "    | file : rest -> do"
        , "        edit file ?"
        , "        editAll rest"
        , "" ] <> checkLines
      -- ANCHOR_END: editLines
    -- ANCHOR: checkLines
    -- Strict: read every file to the end first. A broken byte in any
    -- of them stops it before one file is changed.
    -- Strict なら、先にどのファイルも終わりまで読む。どれかに壊れた
    -- バイトがあれば、1つも書き換えないうちに止まる
    checkLines
      | decodingOf one == Strict =
          [ "-- 🔎 Strict: read every file first. A broken byte in any of"
          , "--    them stops it here, before one file is changed."
          , "-- Strict：先にどのファイルも読む。どれかに壊れたバイトが"
          , "-- あれば、1つも書き換えないうちに、ここで止まる"
          , "checkAll files ="
          , "  case files of"
          , "    | [] -> pure (Ok ())"
          , "    | file : rest -> do"
          , "        _ <- File.forLines Strict file () (\\_ _ _ -> pure ()) ?"
          , "        checkAll rest"
          , "" ]
      | otherwise = []
    -- ANCHOR_END: checkLines
    files = case oneScript one of
      Just PlainForm -> "files"
      _              -> "o.files"
    (signature, start) = case oneScript one of
      Just PlainForm ->
        ( [ "-- 🚪 The files are the arguments (none: the standard input)."
          , "-- 引数がファイル（なければ標準入力）"
          , "main : [String] -> IO (Result String ())" ]
        , "main files =" )
      _ ->
        ( [ "--- Run the one-liner on each file (none: the standard input)"
          , "-- ファイルごとに、ワンライナーを動かす（なければ標準入力）"
          , "main : { files : [String] } -> IO (Result String ())" ]
        , "main o =" )
    orStdin = "(if null " <> files <> " then [\"-\"] else " <> files <> ")"
    mainLines = signature <> case (oneInPlace one, oneEnd one) of
      (Just _, _)
        | decodingOf one == Strict ->
            [ start <> " do"
            , "  checkAll " <> files <> " ?"
            , "  editAll " <> files ]
        | otherwise -> [ start <> " editAll " <> files ]
      (_, Just _) ->
        [ start <> " do"
        , "  last <- readAll start " <> orStdin <> " ?"
        , "  finish last"
        , "  pure (Ok ())" ]
      _ ->
        [ start <> " do"
        , "  _ <- readAll " <> first <> " " <> orStdin <> " ?"
        , "  pure (Ok ())" ]
    first = if folding one then "start" else "()"
-- ANCHOR_END: mainChunks

-- ANCHOR: remap
-- 🧭 An error at a place in the script, told at the place the person
--    wrote:  <one-liner>:12:18  in the piece of -e from line 11, 2 to
--    the right, is  -e:2:16.  Outside the pieces (where the pieces
--    meet), the place is the pieces' names.
-- スクリプトの場所のエラーを、人が書いた場所で言う。かけらの外（かけら
-- どうしが出会う所）なら、場所はかけらの名前で
remap :: OneLiner -> Script -> Text -> Text
remap one script = T.intercalate "\n" . map line . T.lines
  where
    line l = maybe l placed (T.stripPrefix (scriptName <> ":") l >>= numbers)
    placed (row, col, msg0) =
      let msg = ours msg0 in
      case find (inside row) (scriptParts script) of
        Just p -> partName p <> ":" <> number (row - partLine p + 1)
                    <> ":" <> number (max 1 (col - partIndent p))
                    <> ":" <> msg
        Nothing -> outside <> ":" <> msg
    inside row p = row >= partLine p && row < partLine p + partLines p
    numbers t = do
      (row, t1) <- either (const Nothing) Just (TR.decimal t)
      t2 <- T.stripPrefix ":" t1
      (col, t3) <- either (const Nothing) Just (TR.decimal t2)
      t4 <- T.stripPrefix ":" t3
      pure (row, col, t4)
    number = T.pack . show :: Int -> Text
    -- The names of our own definitions say nothing to the person.
    -- こちらの定義の名前は、人には何も言わない
    ours msg = case T.stripPrefix " in the definition of " msg of
      Just more | (_, after) <- T.breakOn ": " more, not (T.null after) ->
        T.drop 1 after
      _ -> msg
    outside = T.intercalate ", " $ concat
      [ [ "--begin" | oneBegin one /= Nothing ]
      , [ "-e" ]
      , [ "--end" | oneEnd one /= Nothing ] ]

-- The name the script is run with.
-- スクリプトを動かすときの名前
scriptName :: Text
scriptName = "<one-liner>"
-- ANCHOR_END: remap

-- ANCHOR: prepare
-- 🔧 Write the script, and let the types decide its shape: load the
--    script without main first, look at the types of step and
--    finish, then write it again with the shape they say.
-- スクリプトを書き、形は型に決めてもらう：まず main なしで読みこみ、
-- step と finish の型を見て、その形でもう一度書く
prepare
  :: Monad m
  => Files m -> Session -> Session -> OneLiner -> m (Either Text Script)
prepare files base session one = case partsParse of
  Left err -> pure (Left err)
  Right () -> do
    first <- probe (Shape False False False)
    case first of
      Left err -> pure (Left err)
      Right s -> do
        let piece = isFunction (after arity (typeOf s "step"))
            acc = isFunction (after 1 (typeOf s "finish"))
            json t = t == TCon "Json" []
                       || (isVar t && oneInput one /= TextLines)
        second <- if piece || acc then probe (Shape piece acc False)
                                  else pure (Right s)
        pure $ do
          s2 <- second
          let final = after arity (typeOf s2 "step")
              shape = Shape piece acc (oneMode one == Printing && json final)
          checkShape final (after 1 (typeOf s2 "finish"))
          Right (scriptOf one shape)
  where
    arity = if folding one then 2 else 1 :: Int
    -- A mistake of syntax is told by the parser, at the person's place.
    -- 書き方のまちがいは、パーサーが、人が書いた場所で言う
    partsParse = do
      let ops = sessionOperators session
      mapM_ (\(name, text) -> parseLam ops (T.unpack name) text)
        ([("-e", T.intercalate "\n" (oneProgram one))]
         <> [("--begin", b) | Just b <- [oneBegin one]]
         <> [("--end", e) | Just e <- [oneEnd one]])
    -- The script without main: the helpers and the pieces.
    -- main のないスクリプト：道具とかけら
    probe shape = do
      let script = probeOf shape
      loaded <- loadProgram files base base
                  (T.unpack scriptName) (scriptText script)
      pure (either (Left . remap one script) (Right . fst) loaded)
    probeOf shape = assemble $
      imports one <> helpers one <> stepChunks one shape
        <> foldChunks one shape <> fits
    -- With --begin, step must give back the type of the first acc.
    -- --begin があれば、step は初めの acc と同じ型を返す
    fits
      | folding one =
          [ Lines [ "fits = if true then start else step start "
                      <> sample ] ]
      | otherwise = []
    sample = case oneInput one of
      TextLines -> "\"\""
      _ -> maybe "JNull" (const "(todo \"\")") (oneAs one)
    typeOf s name = case Map.lookup name (sessionTypes s) of
      Just (Forall _ _ t) -> t
      Nothing             -> TVar 0
    -- ANCHOR: checkShape
    -- 📐 What each line must give: a String (or a Json) for -p, an IO
    --    for -n; with --begin, the acc; and --end must be an IO.
    -- 1行ごとに返すもの：-p なら String（か Json）、-n なら IO。--begin
    -- があれば acc。--end は IO
    checkShape t end
      | folding one, oneEnd one /= Nothing, not (isIO end || isVar end) =
          Left ("--end: type error: --end needs an IO, but this gives "
                  <> render end)
      | folding one = Right ()
      | oneMode one == Printing
      , not (t == tString || t == TCon "Json" [] || isVar t) =
          Left ("-e: type error: -p needs a String (or a Json) for each "
                  <> pieceName one <> ", but this gives " <> render t
                  <> " (for an IO, use -n)")
      | oneMode one == EachLine, not (isIO t || isVar t) =
          Left ("-e: type error: -n needs an IO for each "
                  <> pieceName one <> ", but this gives " <> render t
                  <> " (to print each " <> pieceName one <> ", use -p)")
      | otherwise = Right ()
    -- ANCHOR_END: checkShape
    after :: Int -> Type -> Type
    after n t = case (n, t) of
      (0, _)          -> t
      (_, TFun _ res) -> after (n - 1) res
      _               -> t
    isFunction t = case t of
      TFun _ _ -> True
      _        -> False
    isIO t = case t of
      TCon "IO" _ -> True
      _           -> False
    isVar t = case t of
      TVar _ -> True
      _      -> False
    render t = renderScheme (closeOver t)
-- ANCHOR_END: prepare

-- ANCHOR: runOneLiner
-- 🏃 Run a one-liner: -e alone is a whole program, as if it were a
--    file named -e. With -n or -p, write the script and run it, as
--    lam runs any file; its errors are told at the person's places.
--    With --print-script, only show it.
-- ワンライナーを動かす。-e だけなら、-e という名前のファイルと同じく
-- プログラム全体。-n か -p なら、スクリプトを書いて、どのファイルとも
-- 同じく動かす。エラーは人が書いた場所で言う。--print-script なら、
-- 見せるだけ
runOneLiner
  :: Concurrent m => Handler m -> Files m -> Session -> OneLiner -> m Ending
runOneLiner handle files base one = do
  withModules <- foldM addImport (Right base) (oneModules one)
  case withModules of
    Left err -> pure (Errored err)
    Right session
      | oneMode one == Once && oneScript one == Nothing ->
          runOnce session
      | otherwise -> do
          prepared <- prepare files base session one
          case prepared of
            Left err -> pure (Errored (T.stripEnd err))
            -- ANCHOR: printScript
            -- --print-script: the same script, shown instead of run.
            -- --print-script：同じスクリプトを、動かす代わりに見せる
            Right script
              | Just _ <- oneScript one -> do
                  _ <- handle (PutStr (scriptText script))
                  pure (Finished "")
              | otherwise -> runScript script
            -- ANCHOR_END: printScript
  where
    -- -MPrice, as the line import Price (..) of a file.
    -- -MPrice を、ファイルの import Price (..) の行と同じく
    addImport acc m = case acc of
      Left err -> pure (Left err)
      Right session ->
        case runLamParser (sessionOperators session)
               (spaces *> importLine) "-M" (importText m) of
          Left err -> pure (Left (T.pack (errorBundlePretty err)))
          Right imp -> importAt files base session imp
    runOnce session = do
      loaded <- loadProgram files base session "-e"
                  (T.intercalate "\n" (oneProgram one))
      case loaded of
        Left err -> pure (Errored (T.stripEnd err))
        Right (done, final) ->
          runProgramWith (showValueOnly handle) handle done final
    runScript script = do
      loaded <- loadProgram files base base (T.unpack scriptName)
                  (scriptText script)
      ending <- case loaded of
        Left err             -> pure (Errored err)
        Right (done, final)  -> runProgram handle done final
      pure $ case ending of
        Errored err -> Errored (T.stripEnd (remap one script err))
        other       -> other
-- ANCHOR_END: runOneLiner

-- ANCHOR: showValueOnly
-- 👀 lam -e '1 + 2' shows 3: an expression that is not IO is printed
--    with print (display, without the type). An IO is done, and what
--    it gives is printed, too (but not ()):  getArgs  shows ["a", "b"].
-- IO でない式は print で見せる（display で、型なし）。IO ならやって、
-- くれた値も print で見せる（() は見せない）
showValueOnly :: Concurrent m => Handler m -> Session -> Expr -> m Ending
showValueOnly handle session e =
  case inferType (sessionData session) (sessionTypes session) e of
    Right (Forall _ _ (TCon "IO" [r]), _)
      | r == tUnit || isVar r -> evaluate handle session e
      | otherwise -> evaluate handle session
                       (App (App (Var "bind") e) (Var "print"))
    _ -> evaluate handle session (App (Var "print") e)
  where
    isVar t = case t of
      TVar _ -> True
      _      -> False
-- ANCHOR_END: showValueOnly

-- ANCHOR_END: all
