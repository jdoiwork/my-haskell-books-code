-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The heart of the REPL: one line in, a new session and a text out.
-- REPL の心臓：1行を受け取り、新しいセッションと文字を返す。
module Lam.Repl
  ( Input (..)
  , parseInput
  , step
  , helpText
  ) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec

import Lam.Io (Handler)
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
  | Help                 -- :help
  deriving (Eq, Show)

-- 📖 A definition, a command, or an expression.
-- 定義、コマンド、または式
parseInput :: Text -> Either Text Input
parseInput line =
  case runLamParser (spaces *> input <* eof) "<repl>" line of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right i  -> Right i
  where
    input = choice
      [ ShowNames <$ symbol ":env"
      , Help <$ symbol ":help"
      , ShowType <$> (symbol ":type" *> expr)
      , ShowDicts <$> (symbol ":dicts" *> expr)
      , PrintText <$> (symbol ":print" *> expr)
      , Load . T.unpack . T.strip <$> (symbol ":load" *> takeRest)
      , Browse <$> (symbol ":browse" *> conName)
      , ImportLine <$> importLine
      , DefineType <$> dataDecl
      , DefineAlias <$> aliasDecl
      , DefineClass <$> classDecl
      , DefineInstance <$> instanceDecl
      , typed
      , try (uncurry Define <$> topDefinition <* eof)
      , Evaluate <$> expr ]
    typed = do
      sig <- try (signatureHead <* symbol ";")
      (x, e) <- topDefinition
      case sig of
        DefSig y s | x == y -> pure (DefineTyped x s e)
        -- 「注釈と定義の名前がちがいます」
        _ -> fail "the annotation and the definition have different names"
-- ANCHOR_END: input

-- ANCHOR: step
-- 🔁 One turn of the REPL. (:load is done by Main, which reads files.)
--    An IO expression is done by the handler, so step runs in its
--    monad; everything else is answered as before.
-- REPL の1回分（:load は、ファイルを読む Main がする）。IO の式は
-- ハンドラーがやるので、step はそのモナドの中で動く。ほかは前と同じ
step :: Monad m => Handler m -> Session -> Text -> m (Session, Text)
step handle session line =
  case parseInput line of
    Right (Evaluate e) -> do
      out <- evaluate handle session e
      pure (session, either id id out)
    other -> pure (answer session other)

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
    Right (Load file) ->
      (session, "(:load " <> T.pack file <> " is handled by the REPL)")
    Right (ImportLine _) ->
      (session, "(import is handled by the REPL)")
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
  <> [ "(hidden: " <> T.intercalate ", " secret <> ")"
     | let secret = concatMap snd (Map.elems (moduleShown md))
     , not (null secret) ]
  <> [ "(instances: " <> T.intercalate ", " own <> ")"
     | let own = [ c <> " " <> t
                 | ((c, t), home) <- Map.toList (moduleInstances md)
                 , home == moduleName md ]
     , not (null own) ]
-- ANCHOR_END: browseLines

-- 📖 :help
helpText :: Text
helpText = T.intercalate "\n"
  [ "name args = expr       define a name (let may be left out)"
  , "name : T; name x = e   define a name with a type annotation"
  , "data T a = C a | D      define a data type"
  , "type alias R = { x : Int }   give a type another name"
  , "class C a where m : a -> T    declare a class (one line here)"
  , "instance C T where m x = e    give a type to a class"
  , "{\"key\": expr, ...}     a Map: {\"Python\": 82}, {1: \"one\"}"
  , "expr                   compute an expression"
  , ":type expr             show the type of an expression"
  , ":dicts expr            show it with the dictionaries put in"
  , "do x <- e; f x         one line after another (an IO is done)"
  , ":print expr            write a String (or a [String]) as it is"
  , "f\"x = {x:>5.1}\"        put values in a string"
  , ":load file.lam         load the definitions in a file"
  , "import M as N (x)      import a module (from here, and LAM_PATH)"
  , ":browse M              list what a module shows"
  , ":env                   list the names you can use"
  , ":quit                  quit" ]
-- ANCHOR_END: step
-- ANCHOR_END: all
