-- ANCHOR: all
-- | Answers to the training of "Functor, Applicative and Monad, deeper".
-- 「Functor・Applicative・Monad をもっと深く」のトレーニングの解答
module DeepAnswers
  ( Pair (..)
  , checkGold
  , applyWithGold
  , Recruit (..)
  ) where

import Validation

-- ANCHOR: pair
-- 👯 Step 2: a pair of two values of the same type.
-- ステップ2：同じ型の値が2つ入った組
data Pair a = Pair a a
  deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)
-- ANCHOR_END: pair

-- ANCHOR: gold
-- 💰 Step 3: one more check, and one more field.
-- ステップ3：確かめることを1つ、フィールドを1つ足す
data Recruit = Recruit
  { applicant :: Applicant
  , fee       :: Int
  }
  deriving (Show, Eq)

checkGold :: Int -> Validation [String] Int
checkGold g
  | g < 100   = Invalid ["not enough gold"]
  | otherwise = Valid g

applyWithGold :: String -> Int -> String -> Int -> Validation [String] Recruit
applyWithGold n a j g =
  Recruit <$> applyAll n a j
          <*> checkGold g
-- ANCHOR_END: gold
-- ANCHOR_END: all
