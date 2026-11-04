{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The heart of the REPL: one line in, a new environment and
--   a text to show out. A pure function, so it is easy to test.
-- REPL の心臓：1行を受け取り、新しい環境と、見せる文字を返す。
-- 純粋な関数なので、テストしやすい。
module Lam.Repl
  ( Input (..)
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

-- ANCHOR: input
-- ⌨️ What the user typed.
-- ユーザーが打ったもの
data Input
  = Define Name Expr   -- let double = \x -> x * 2   （in がない）
  | Evaluate Expr      -- double 21
  | ShowNames          -- :env
  deriving (Eq, Show)

-- 📖 A definition (let without in), a command, or an expression.
-- 定義（in のない let）、コマンド、または式
parseInput :: Text -> Either Text Input
parseInput line =
  case parse (spaces *> input <* eof) "<repl>" line of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right i  -> Right i
  where
    input = choice
      [ ShowNames <$ symbol ":env"
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
-- 🔁 One turn of the REPL.
-- REPL の1回分
step :: Env -> Text -> (Env, Text)
step env line =
  case parseInput line of
    Left err -> (env, err)
    Right ShowNames ->
      (env, T.unwords (Map.keys env))
    Right (Evaluate e) ->
      case eval env e of
        Left err -> (env, "error: " <> renderError err)
        Right v  -> (env, renderValue v)
    Right (Define x e) ->
      -- The same knot as let: the definition can call itself.
      -- let と同じ結び目：定義は、自分自身を呼べる
      let env'   = Map.insert x v env
          result = eval env' e
          v      = either (const (VInt 0)) id result
      in case result of
           Left err -> (env, "error: " <> renderError err)
           Right _  -> (env', x <> " = " <> renderValue v)
-- ANCHOR_END: step

-- ANCHOR: run
-- 📜 Run a whole program file: parse it, then compute it.
-- プログラムのファイル全体を動かす：読んでから、計算する
runProgram :: FilePath -> Text -> Either Text Text
runProgram file source = do
  e <- parseLam file source
  case eval Map.empty e of
    Left err -> Left ("error: " <> renderError err)
    Right v  -> Right (renderValue v)
-- ANCHOR_END: run
-- ANCHOR_END: all
