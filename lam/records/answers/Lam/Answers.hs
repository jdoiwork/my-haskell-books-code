-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | The answers of the training of "Writing types: type annotations
--   and records".
-- 「型を書く：型の注釈とレコード」のトレーニングの解答。
module Lam.Answers
  ( shortRecord
  ) where

import qualified Data.Text as T
import Text.Megaparsec (sepBy1, try)

import Lam.Parser (Parser, name, symbol)
import Lam.Syntax

-- ANCHOR: shortRecord
-- ✂️ Step 5: { name, days } is short for { name = name, days = days },
--    as in JavaScript. Each field gets the variable of the same name.
--    Put it into the choice of atom, after recordLiteral: a map always
--    has : after its keys, so { name, days } is never one.
-- ステップ5：{ name, days } は { name = name, days = days } の略
-- （JavaScript と同じ）。どのフィールドも、同じ名前の変数の値になる。
-- atom の choice の recordLiteral の後ろに置く。Map はキーの後ろに必ず :
-- があるので、{ name, days } が Map になることはない
shortRecord :: Parser Expr
shortRecord = do
  xs <- try (symbol "{" *> (name `sepBy1` symbol ",") <* symbol "}")
  case [ x | (i, x) <- zip [0 :: Int ..] xs, x `elem` drop (i + 1) xs ] of
    -- 「フィールド days が2度あります」
    x : _ -> fail ("the field " <> T.unpack x <> " is written twice")
    []    -> pure (Record [ (x, Var x) | x <- xs ])
-- ANCHOR_END: shortRecord
-- ANCHOR_END: all
