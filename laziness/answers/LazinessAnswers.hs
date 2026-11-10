-- ANCHOR: all
-- 🏋️ Answers for the training.
-- トレーニングの解答
module LazinessAnswers (average, countHits) where

-- ANCHOR: imports
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
-- ANCHOR_END: imports

-- ANCHOR: average
-- 📊 A running sum and count, both worked out at each step.
-- 途中の合計と個数（どちらも、毎回その場で計算する）
data SumCount = SumCount !Int !Int

-- ➗ The average of the damages, in one pass over the list.
-- ダメージの平均（リストを1回たどるだけで）
average :: [Int] -> Double
average ds =
  let SumCount s c = foldl' step (SumCount 0 0) ds
  in  if c == 0 then 0 else fromIntegral s / fromIntegral c
  where
    step (SumCount s c) d = SumCount (s + d) (c + 1)
-- ANCHOR_END: average

-- ANCHOR: count
-- 🗂️ How many hits each hero made. Data.Map.Strict adds at once.
-- 勇者ごとの攻撃の回数。Data.Map.Strict は、その場で足す
countHits :: [String] -> Map String Int
countHits heroes = foldl' add Map.empty heroes
  where
    add counts hero = Map.insertWith (+) hero 1 counts
-- ANCHOR_END: count
-- ANCHOR_END: all
