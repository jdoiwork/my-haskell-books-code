-- ANCHOR: pragmas
{-# LANGUAGE OverloadedLabels #-}
-- ANCHOR_END: pragmas
-- ANCHOR: all
-- | Answers to the training of "A small typed SQL".
-- 「小さな型安全 SQL を作る」のトレーニングの解答
module SqlAnswers
  ( between
  , middleLevels
  ) where

-- ANCHOR: imports
import Data.Function ((&))

import Guild
import MiniSql.Expr
import MiniSql.Query
-- ANCHOR_END: imports

-- ANCHOR: between
-- ↔️ Step 2: lo <= e AND e <= hi, built from the pieces we have.
-- ステップ2：lo <= e AND e <= hi を、手持ちの部品で
between :: Expr t Int -> Int -> Int -> Expr t Bool
between e lo hi = (lit lo .<= e) .&& (e .<= lit hi)

middleLevels :: Select Adventurer
middleLevels =
  selectFrom adventurers
    & where_ (between #level 3 5)
    & orderBy #level Asc
-- ANCHOR_END: between
-- ANCHOR_END: all
