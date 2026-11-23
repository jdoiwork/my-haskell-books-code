-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The heart of the REPL: one line in, a new session and a text out.
-- REPL の心臓：1行を受け取り、新しいセッションと文字を返す。
module Lam.Repl
  ( Input (..)
  , parseInput
  , step
  , endingText
  , helpText
  , showFixity
  , withIt
  , completions
  ) where

import Data.Char (isAlphaNum)
import Data.List (sortOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec

import Lam.Eval (Request (..), Value (..))
import Lam.Io (Concurrent, Handler)
import Lam.Parser
import Lam.Pretty (render)
import Lam.Program
import Lam.Syntax
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: input
-- ⌨️ What the user typed.
-- ユーザーが打ったもの
data Input
  = Define Name Expr     -- let double x = x * 2
  | DefineType DataDecl  -- data Shape = Circle Int | Rect Int Int
  -- ANCHOR: replRecords
  | DefineAlias AliasDecl  -- type alias Member = { name : String }
  -- add : Int -> Int -> Int; add x y = x + y   (; joins the two lines)
  -- （; で2行をつなぐ）
  | DefineTyped Name Signature Expr
  -- ANCHOR_END: replRecords
  -- ANCHOR: replClasses
  | DefineClass ClassDecl        -- class Describe a where describe : a -> String
  | DefineInstance InstanceDecl  -- instance Describe Int where describe n = ...
  -- ANCHOR_END: replClasses
  -- ANCHOR: replInfix
  | DefineInfix Text Fixity      -- infixl 7 addPrice as <+>
  -- ANCHOR_END: replInfix
  | Evaluate Expr        -- double 21
  | ShowType Expr        -- :type double
  -- ANCHOR: replDicts
  | ShowDicts Expr       -- :dicts describe (Circle 2)
  -- ANCHOR_END: replDicts
  -- ANCHOR: replPrint
  | PrintText Expr       -- :print receipt
  -- ANCHOR_END: replPrint
  -- ANCHOR: importInput
  | ImportLine Import    -- import Json as J
  | Browse Name          -- :browse Json
  -- ANCHOR_END: importInput
  | ShowNames            -- :env
  | Load FilePath        -- :load examples/fib.lam
  -- ANCHOR: replTestingInput
  | Reload               -- :reload (or :r): the last file, again
  | RunTests [String]    -- :test words_test.lam  (the words of lam test)
  -- ANCHOR_END: replTestingInput
  | Help                 -- :help
  deriving (Eq, Show)

-- 📖 A definition, a command, or an expression.
-- 定義、コマンド、または式
parseInput :: Operators -> Text -> Either Text Input
parseInput ops line =
  case runLamParser ops (spaces *> input <* eof) "<repl>" line of
    Left err -> Left (T.pack (errorBundlePretty err))
    -- One line is right in front of you: the errors need no places.
    -- 1行は目の前にあるので、エラーに場所は要らない
    Right i  -> Right (withoutPlaces i)
  where
    input = choice
      [ ShowNames <$ symbol ":env"
      , Help <$ symbol ":help"
      -- :t is short for :type (as in GHCi); :test is not :t.
      -- :t は :type の短い形（GHCi と同じ）。:test は :t ではない
      , ShowType <$> (try (symbol ":type" <|> shortType) *> expr)
      , ShowDicts <$> (symbol ":dicts" *> expr)
      , PrintText <$> (symbol ":print" *> expr)
      , Load . T.unpack . T.strip <$> (symbol ":load" *> takeRest)
      -- ANCHOR: replTestingParse
      , Reload <$ (try (symbol ":reload") <|> try (symbol ":r" <* eof))
      -- Not symbol: -- would be read as a comment (--seeds 40).
      -- symbol ではない。-- がコメントと読まれてしまう（--seeds 40）
      , RunTests . map T.unpack . T.words <$> (chunk ":test" *> takeRest)
      -- ANCHOR_END: replTestingParse
      , Browse <$> (symbol ":browse" *> conName)
      , ImportLine <$> importLine
      , DefineType <$> dataDecl
      , DefineAlias <$> aliasDecl
      , DefineClass <$> classDecl
      , DefineInstance <$> instanceDecl
      , infixInput
      , typed
      , try (uncurry Define <$> topDefinition <* eof)
      , Evaluate <$> expr ]
    shortType = try (chunk ":t" <* notFollowedBy (satisfy isAlphaNum))
                  <* spaces
    infixInput = do
      d <- infixDecl
      case d of
        DefInfix _ o f -> pure (DefineInfix o f)
        _              -> empty
    typed = do
      sig <- try (signatureHead <* symbol ";")
      (x, e) <- topDefinition
      case sig of
        DefSig y s | x == y -> pure (DefineTyped x s e)
        -- 「注釈と定義の名前がちがいます」
        _ -> fail "the annotation and the definition have different names"

-- 🧽 The same input, without the marks of places.
-- 同じ入力から、場所の印を取る
withoutPlaces :: Input -> Input
withoutPlaces i = case i of
  Define x e        -> Define x (stripLocations e)
  DefineTyped x s e -> DefineTyped x s (stripLocations e)
  DefineClass c     -> case stripDecl (DefClass c) of
    DefClass c' -> DefineClass c'
    _           -> DefineClass c
  DefineInstance d  -> case stripDecl (DefInstance d) of
    DefInstance d' -> DefineInstance d'
    _              -> DefineInstance d
  Evaluate e        -> Evaluate (stripLocations e)
  ShowType e        -> ShowType (stripLocations e)
  ShowDicts e       -> ShowDicts (stripLocations e)
  PrintText e       -> PrintText (stripLocations e)
  other             -> other
-- ANCHOR_END: input

-- ANCHOR: step
-- 🔁 One turn of the REPL. (:load is done by Main, which reads files.)
--    An IO expression is done by the handler, so step runs in its
--    monad; everything else is answered as before.
-- REPL の1回分（:load は、ファイルを読む Main がする）。IO の式は
-- ハンドラーがやるので、step はそのモナドの中で動く。ほかは前と同じ
step :: Concurrent m => Handler m -> Session -> Text -> m (Session, Text)
step handle session line = do
  let input = parseInput (sessionOperators session) line
  -- ANCHOR: lineWarnings
  -- A warning goes to the standard error first; the line still runs.
  -- 警告を先に標準エラーへ。行は、それでも動かす
  mapM_ (\w -> handle (PutErr (w <> "\n"))) (lineWarnings session input)
  -- ANCHOR_END: lineWarnings
  case input of
    -- ANCHOR: stepIt
    -- The value of an expression is kept as it, for the next line.
    -- 式の値を、次の行のために it として取っておく
    Right (Evaluate e) -> do
      (out, value) <- evaluateIt handle session e
      pure (withIt value session, endingText out)
    -- ANCHOR_END: stepIt
    other -> pure (owning other (answer session other))
  where
    -- A name defined in the REPL is the user's own (see keyWarnings).
    -- REPL で定義した名前は、自分のもの（keyWarnings を見る）
    owning input (s, shown) = case defined input of
      Just x  -> (s { sessionOwn = Set.insert x (sessionOwn s) }, shown)
      Nothing -> (s, shown)

-- ⚠️ The warnings of one line of the REPL (see keyWarnings).
-- REPL の1行の警告（keyWarnings を見る）
lineWarnings :: Session -> Either Text Input -> [Text]
lineWarnings session input = case input of
  Right (Evaluate e)        -> keyWarnings own e
  Right (Define x e)        -> keyWarnings (Set.insert x own) e
  Right (DefineTyped x _ e) -> keyWarnings (Set.insert x own) e
  _                         -> []
  where
    own = sessionOwn session

-- 🏷️ The name a line defines, if it defines one.
-- 行が定義する名前（あれば）
defined :: Either Text Input -> Maybe Name
defined input = case input of
  Right (Define x _)        -> Just x
  Right (DefineTyped x _ _) -> Just x
  _                         -> Nothing

-- ANCHOR: endingText
-- 🏁 What the REPL shows for an ending. exit does not end the REPL:
--    :quit does.
-- REPL が見せるもの。exit では REPL は終わらない（終わるのは :quit）
endingText :: Ending -> Text
endingText ending = case ending of
  Finished t   -> t
  Errored t    -> t
  -- 「（exit 2。REPL は続きます）」
  ExitedWith n -> "(exit " <> T.pack (show n) <> "; the REPL goes on)"
-- ANCHOR_END: endingText

-- 💬 The answer to everything but an expression: no IO here.
-- 式のほかの全部への答え。ここに IO はない
answer :: Session -> Either Text Input -> (Session, Text)
answer session input =
  case input of
    Left err -> (session, err)
    Right Help -> (session, helpText)
    Right ShowNames ->
      (session, T.unwords (filter (not . T.isPrefixOf "$")
                             (Map.keys (sessionValues session))))
    Right (ShowType e) ->
      case inferType (sessionData session) (sessionTypes session) e of
        Left err -> (session, "type error: " <> renderTypeError err)
        Right (t, _) -> (session, renderUsual t)
    -- ANCHOR: stepDicts
    -- The expression the evaluator gets: with the dictionaries put in.
    -- 評価器が受け取る式：辞書を入れたもの
    Right (ShowDicts e) ->
      case inferType (sessionData session) (sessionTypes session) e of
        Left err      -> (session, "type error: " <> renderTypeError err)
        Right (_, e') -> (session, render e')
    -- ANCHOR_END: stepDicts
    Right (Evaluate _) -> (session, "(an expression is done by step)")
    -- ANCHOR: stepPrint
    Right (PrintText e) -> (session, either id id (printText session e))
    -- ANCHOR_END: stepPrint
    Right (Define x e) -> case define session x e of
      Left err           -> (session, err)
      Right (s', shown)  -> (s', shown)
    Right (DefineType d) -> case defineData session d of
      Left err           -> (session, err)
      Right (s', shown)  -> (s', shown)
    -- ANCHOR: stepRecords
    Right (DefineAlias a) -> case defineAlias session a of
      Left err           -> (session, err)
      Right (s', shown)  -> (s', shown)
    Right (DefineTyped x sig e) -> case defineTyped session x sig e of
      Left err           -> (session, err)
      Right (s', shown)  -> (s', shown)
    -- ANCHOR_END: stepRecords
    -- ANCHOR: stepClasses
    Right (DefineClass c) -> case defineClass session c of
      Left err           -> (session, err)
      Right (s', shown)  -> (s', shown)
    Right (DefineInstance i) -> case defineInstance session i of
      Left err           -> (session, err)
      Right (s', shown)  -> (s', shown)
    -- ANCHOR_END: stepClasses
    -- ANCHOR: stepInfix
    Right (DefineInfix o f) ->
      (defineInfix session o f, showFixity o f)
    -- ANCHOR_END: stepInfix
    Right (Load file) ->
      (session, "(:load " <> T.pack file <> " is handled by the REPL)")
    Right (ImportLine _) ->
      (session, "(import is handled by the REPL)")
    Right Reload -> (session, "(:reload is handled by the REPL)")
    Right (RunTests _) -> (session, "(:test is handled by the REPL)")
    -- ANCHOR: browse
    -- What a module shows: its names and types, what it hides, and
    -- the instances it declares.
    -- モジュールが見せるもの：名前と型、隠しているもの、宣言したインスタンス
    Right (Browse m) -> case Map.lookup m (sessionModules session) of
      Nothing -> (session, "no module " <> m <> " (import it first)")
      Just md -> (session, T.intercalate "\n" (browse md))
    -- ANCHOR_END: browse

-- ANCHOR: browseLines
-- 📖 The lines of :browse:  m.x : type, the hidden constructors, and
--    the instances.
-- :browse の行
browse :: Module -> [Text]
browse md =
  [ moduleName md <> "." <> x <> " : " <> renderUsual sc
  | (x, (sc, _)) <- Map.toList (moduleExports md) ]
  <> [ showFixity o f
     | (o, f) <- sortOn (\(o, f) -> (negate (fixityLevel f), o))
                   (Map.toList (moduleOperators md)) ]
  <> [ "(hidden: " <> T.intercalate ", " secret <> ")"
     | let secret = concatMap snd (Map.elems (moduleShown md))
     , not (null secret) ]
  <> [ "(instances: " <> T.intercalate ", " own <> ")"
     | let own = [ c <> " " <> t
                 | ((c, t), home) <- Map.toList (moduleInstances md)
                 , home == moduleName md ]
     , not (null own) ]
-- ANCHOR_END: browseLines

-- ANCHOR: withIt
-- 🔖 Keep a value as it (a name like any other), unless there is none
--    (an error, exit) or it is () (an action that only did something).
-- 値を it として取っておく（ほかと同じ、ただの名前）。値がない（エラー、
-- exit）か、() （何かをしただけの頼みごと）なら、そのまま
withIt :: Maybe (Scheme, Value) -> Session -> Session
withIt value session = case value of
  Just (_, VTuple []) -> session
  Just (sc, v)        -> session
    { sessionTypes  = Map.insert "it" sc (sessionTypes session)
    , sessionValues = Map.insert "it" v (sessionValues session) }
  Nothing             -> session

-- ANCHOR: completions
-- ⌨️ What Tab can complete: the commands, and the names a program can
--    write (not $fmt.display and the like).
-- Tab で補えるもの：コマンドと、プログラムが書ける名前（$fmt.display
-- などは除く）
completions :: Session -> [Text]
completions session =
  [ ":browse", ":dicts", ":env", ":help", ":load", ":print", ":quit"
  , ":reload", ":test", ":type" ]
    <> filter (not . T.isPrefixOf "$") (Map.keys (sessionValues session))
-- ANCHOR_END: completions
-- ANCHOR_END: withIt

-- ANCHOR: showFixity
-- ➕ infixl 7 addPrice as <+>
showFixity :: Text -> Fixity -> Text
showFixity o f =
  word (fixityAssoc f) <> " " <> T.pack (show (fixityLevel f)) <> " "
    <> fixityTarget f <> " as " <> o
  where
    word a = case a of
      LeftA  -> "infixl"
      RightA -> "infixr"
      NoneA  -> "infix"
-- ANCHOR_END: showFixity

-- 📖 :help
helpText :: Text
helpText = T.intercalate "\n"
  [ "name args = expr       define a name (let may be left out)"
  , "name : T; name x = e   define a name with a type annotation"
  , "data T a = C a | D      define a data type"
  , "type alias R = { x : Int }   give a type another name"
  , "class C a where m : a -> T    declare a class (one line here)"
  , "instance C T where m x = e    give a type to a class"
  , "infixl 7 f as <+>      make <+> another name for f (0 to 9)"
  , "{\"key\": expr, ...}     a Map: {\"Python\": 82}, {1: \"one\"}"
  , "expr                   compute an expression"
  , ":type expr  (:t)       show the type of an expression"
  , ":dicts expr            show it with the dictionaries put in"
  , "do x <- e; f x         one line after another (an IO is done)"
  , "do x <- e ?; ...       a failure (Nothing, Err) is the answer at once"
  , "do defer c; ...        do c at the end, whatever happens (IO)"
  , ":print expr            write a String (or a [String]) as it is"
  , "f\"x = {x:>5.1}\"        put values in a string"
  , "re\"\\d+\"  m/\\d+/i  s/a/b/g    a regex, and a replacing function"
  , "Parser.run p text      read text with a parser (do x <- p; ...)"
  , "Http.getAs d url       ask the web, and read the JSON with a decoder"
  , "Sql.withDb file f      open a database, use it, and close it"
  , ":load file.lam         load the definitions in a file"
  , ":reload (:r)           load the last file again"
  , ":test [files]          run the tests (as lam test does)"
  , "it                     the value of the last expression"
  , "Tab                    complete a name, a command, or a file"
  , "import M as N (x)      import a module (from here, and LAM_PATH)"
  , ":browse M              list what a module shows"
  , ":env                   list the names you can use"
  , ":quit                  quit" ]
-- ANCHOR_END: step
-- ANCHOR_END: all
