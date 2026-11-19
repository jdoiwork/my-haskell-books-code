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
      , Load . T.unpack . T.strip <$> (symbol ":load" *> takeRest)
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
-- REPL の1回分（:load は、ファイルを読む Main がする）
step :: Session -> Text -> (Session, Text)
step session line =
  case parseInput line of
    Left err -> (session, err)
    Right Help -> (session, helpText)
    Right ShowNames ->
      (session, T.unwords (filter (not . T.isPrefixOf "$")
                             (Map.keys (sessionValues session))))
    Right (ShowType e) ->
      case inferType (sessionData session) (sessionTypes session) e of
        Left err -> (session, "type error: " <> renderTypeError err)
        Right (t, _) -> (session, renderScheme t)
    -- ANCHOR: stepDicts
    -- The expression the evaluator gets: with the dictionaries put in.
    -- 評価器が受け取る式：辞書を入れたもの
    Right (ShowDicts e) ->
      case inferType (sessionData session) (sessionTypes session) e of
        Left err      -> (session, "type error: " <> renderTypeError err)
        Right (_, e') -> (session, render e')
    -- ANCHOR_END: stepDicts
    Right (Evaluate e) -> (session, either id id (evaluate session e))
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

-- 📖 :help
helpText :: Text
helpText = T.intercalate "\n"
  [ "name args = expr       define a name (let may be left out)"
  , "name : T; name x = e   define a name with a type annotation"
  , "data T a = C a | D      define a data type"
  , "type alias R = { x : Int }   give a type another name"
  , "class C a where m : a -> T    declare a class (one line here)"
  , "instance C T where m x = e    give a type to a class"
  , "expr                   compute an expression"
  , ":type expr             show the type of an expression"
  , ":dicts expr            show it with the dictionaries put in"
  , ":load file.lam         load the definitions in a file"
  , ":env                   list the names you can use"
  , ":quit                  quit" ]
-- ANCHOR_END: step
-- ANCHOR_END: all
