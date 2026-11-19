-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Answers to the training of the chapter on type annotations and
--   records.
-- 型の注釈とレコードの章のトレーニングの解答。
module Lam.Answers
  ( shortRecord
  ) where

import qualified Data.Text as T
import Text.Megaparsec (sepBy1, try)

import Lam.Parser (Parser, name, symbol)
import Lam.Syntax
-- ANCHOR_END: head

-- ANCHOR: shortRecord
-- ✂️ Step 5: { name, days } is short for { name = name, days = days },
--    as in JavaScript. Each field gets the variable of the same name.
--    Put it into the choice of atom, after recordLiteral: a record
--    always has = after its first field, so recordLiteral reads
--    nothing of { name, days }.
-- ステップ5：{ name, days } は { name = name, days = days } の略
-- （JavaScript と同じ）。どのフィールドも、同じ名前の変数の値になる。
-- atom の choice の recordLiteral の後ろに置く
shortRecord :: Parser Expr
shortRecord = do
  xs <- try (symbol "{" *> (name `sepBy1` symbol ",") <* symbol "}")
  case [ x | (i, x) <- zip [0 :: Int ..] xs, x `elem` drop (i + 1) xs ] of
    -- 「フィールド days が2度あります」
    x : _ -> fail ("the field " <> T.unpack x <> " is written twice")
    []    -> pure (Record [ (x, Var x) | x <- xs ])
-- ANCHOR_END: shortRecord
-- ANCHOR_END: all
