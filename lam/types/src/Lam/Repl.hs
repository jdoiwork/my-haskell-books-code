-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The heart of the REPL: one line in, a new session and a text out.
--   Now every line is type-checked before it runs.
-- REPL の心臓：1行を受け取り、新しいセッションと文字を返す。
-- この章から、どの行も、動かす前に型を確かめる。
module Lam.Repl
  ( Session (..)
  , emptySession
  , Input (..)
  , parseInput
  , step
  , runProgram
  ) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec

import Lam.Eval
import Lam.Parser
import Lam.Syntax
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: session
-- 🗂️ What the REPL remembers: the type and the value of each name.
-- REPL が覚えていること：それぞれの名前の型と値
data Session = Session
  { sessionTypes  :: TypeEnv
  , sessionValues :: Env
  }

emptySession :: Session
emptySession = Session Map.empty Map.empty
-- ANCHOR_END: session

-- ANCHOR: input
-- ⌨️ What the user typed.
-- ユーザーが打ったもの
data Input
  = Define Name Expr   -- let double = \x -> x * 2
  | Evaluate Expr      -- double 21
  | ShowType Expr      -- :type double
  | ShowNames          -- :env
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
      , ShowType <$> (symbol ":type" *> expr)
      , try definition
      , Evaluate <$> expr ]
    definition = do
      keyword "let"
      x <- name
      _ <- symbol "="
      e <- expr
      eof
      pure (Define x e)
-- ANCHOR_END: input

-- ANCHOR: step
-- 🔁 One turn of the REPL: check the type first, then compute.
-- REPL の1回分：まず型を確かめ、それから計算する
step :: Session -> Text -> (Session, Text)
step session line =
  case parseInput line of
    Left err -> (session, err)
    Right ShowNames ->
      (session, T.unwords (Map.keys (sessionValues session)))
    Right (ShowType e) ->
      case inferType (sessionTypes session) e of
        Left err -> (session, "type error: " <> renderTypeError err)
        Right t  -> (session, renderType t)
    Right (Evaluate e) ->
      case inferType (sessionTypes session) e of
        Left err -> (session, "type error: " <> renderTypeError err)
        Right t  -> case eval (sessionValues session) e of
          Left err -> (session, "error: " <> renderError err)
          Right v  -> (session, renderValue v <> " : " <> renderType t)
    Right (Define x e) ->
      case inferDefinition (sessionTypes session) x e of
        Left err -> (session, "type error: " <> renderTypeError err)
        Right sc ->
          let env'   = Map.insert x v (sessionValues session)
              result = eval env' e
              v      = either (const (VInt 0)) id result
          in case result of
               Left err -> (session, "error: " <> renderError err)
               Right _  ->
                 ( Session (Map.insert x sc (sessionTypes session)) env'
                 , x <> " : " <> renderScheme sc )
-- ANCHOR_END: step

-- ANCHOR: run
-- 📜 Run a whole program file: parse, check the type, then compute.
-- プログラムのファイル全体を動かす：読み、型を確かめ、計算する
runProgram :: FilePath -> Text -> Either Text Text
runProgram file source = do
  e <- parseLam file source
  t <- either (Left . ("type error: " <>) . renderTypeError) Right
              (inferType Map.empty e)
  v <- either (Left . ("error: " <>) . renderError) Right
              (eval Map.empty e)
  pure (renderValue v <> " : " <> renderType t)
-- ANCHOR_END: run
-- ANCHOR_END: all
