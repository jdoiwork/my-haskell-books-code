-- ANCHOR: all
-- | Answers to the training of "Basic statistics".
-- 「統計の基本」のトレーニングの解答
module StatsAnswers
  ( trimmedMean
  , outliers
  ) where

import Data.List (sort)

import Math.Stats

-- ANCHOR: trimmed
-- ✂️ Step 2: drop the smallest and largest k values, then take the mean.
-- ステップ2：小さいほうと大きいほうから k 個ずつ捨ててから、平均
trimmedMean :: Int -> [Double] -> Double
trimmedMean k xs =
  let kept = take (length xs - 2 * k) (drop k (sort xs))
  in  mean kept
-- ANCHOR_END: trimmed

-- ANCHOR: outliers
-- 🚨 Step 3: values more than k standard deviations away from the mean.
-- ステップ3：平均から、標準偏差の k 倍より遠い値
outliers :: Double -> [Double] -> [Double]
outliers k xs =
  let m = mean xs
      s = stdDev xs
  in  [ x | x <- xs, abs (x - m) > k * s ]
-- ANCHOR_END: outliers
-- ANCHOR_END: all
