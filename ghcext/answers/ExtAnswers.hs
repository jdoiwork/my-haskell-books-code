-- ANCHOR: pragmas
{-# LANGUAGE DerivingVia #-}
-- ANCHOR_END: pragmas
-- ANCHOR: all
-- | Answers to the training of "GHC extensions".
-- 「GHC 拡張」のトレーニングの解答
module ExtAnswers (Lowest (..)) where

-- ANCHOR: imports
import Data.Semigroup (Min (..))
-- ANCHOR_END: imports

-- ANCHOR: lowest
-- 🐢 Step 2: the lowest price wins: borrow from Min Int.
-- ステップ2：いちばん安い値段を残す：Min Int から借りる
newtype Lowest = Lowest Int
  deriving stock   (Show)
  deriving newtype (Eq)
  deriving (Semigroup, Monoid) via (Min Int)
-- ANCHOR_END: lowest
-- ANCHOR_END: all
