-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | Answers to the training of the chapter on f"...".
-- f"..." の章のトレーニングの解答。
module Lam.Answers
  ( fieldWithRepr
  , formatStringWithRepr
  ) where

import Data.Maybe (fromMaybe, isJust)
import Text.Megaparsec (optional)
import Text.Megaparsec.Char (char, string)

import Lam.Parser
import Lam.Syntax
-- ANCHOR_END: head

-- ANCHOR: repr
-- 🔎 Step 2: {x!r} shows the value as the REPL shows it, with the
--    quotes of a string, like Python's !r:  f"{name!r}" is "\"Lam\"".
--    The format still works after it:  {name!r:>8}.
-- ステップ2：{x!r} は、REPL と同じ形（文字列は引用符つき）で見せる。
-- Python の !r と同じ。後ろに書式も書ける
fieldWithRepr :: Parser Expr
fieldWithRepr = do
  _      <- char '{'
  e      <- spaces *> formatExpr
  repr   <- isJust <$> optional (string "!r")
  format <- optional (char ':' *> formatSpec)
  _      <- char '}'
  let s = fromMaybe noSpec format
  pure $ if repr
    then pad s (call "show" [e])
    else applySpec s e

-- 🪄 f"..." with the field above.
-- 上のフィールドを使う f"..."
formatStringWithRepr :: Parser Expr
formatStringWithRepr = formatStringWith fieldWithRepr
-- ANCHOR_END: repr
-- ANCHOR_END: all
