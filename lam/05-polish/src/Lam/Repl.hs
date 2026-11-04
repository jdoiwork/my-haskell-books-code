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
import Lam.Program
import Lam.Syntax
import Lam.Types

-- ANCHOR: input
-- ⌨️ What the user typed.
-- ユーザーが打ったもの
data Input
  = Define Name Expr   -- let double x = x * 2
  | Evaluate Expr      -- double 21
  | ShowType Expr      -- :type double
  | ShowNames          -- :env
  | Load FilePath      -- :load examples/fib.lam
  | Help               -- :help
  deriving (Eq, Show)

-- 📖 A definition, a command, or an expression.
-- 定義、コマンド、または式
parseInput :: Text -> Either Text Input
parseInput line =
  case parse (spaces *> input <* eof) "<repl>" line of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right i  -> Right i
  where
    input = choice
      [ ShowNames <$ symbol ":env"
      , Help <$ symbol ":help"
      , ShowType <$> (symbol ":type" *> expr)
      , Load . T.unpack . T.strip <$> (symbol ":load" *> takeRest)
      , try (uncurry Define <$> definition <* eof)
      , Evaluate <$> expr ]
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
      (session, T.unwords (Map.keys (sessionValues session)))
    Right (ShowType e) ->
      case inferType (sessionTypes session) e of
        Left err -> (session, "type error: " <> renderTypeError err)
        Right t  -> (session, renderType t)
    Right (Evaluate e) -> (session, either id id (evaluate session e))
    Right (Define x e) -> case define session x e of
      Left err           -> (session, err)
      Right (s', shown)  -> (s', shown)
    Right (Load file) ->
      (session, "(:load " <> T.pack file <> " is handled by the REPL)")

-- 📖 :help
helpText :: Text
helpText = T.intercalate "\n"
  [ "let name args = expr   define a name (it can call itself)"
  , "expr                   compute an expression"
  , ":type expr             show the type of an expression"
  , ":load file.lam         load the definitions in a file"
  , ":env                   list the names you can use"
  , ":quit                  quit" ]
-- ANCHOR_END: step
-- ANCHOR_END: all
