-- ANCHOR: all
-- | Answers to the training of "Exponents and logarithms".
-- 「指数と対数」のトレーニングの解答
module GrowthAnswers
  ( yearsToDouble
  , expFor
  , levelFor
  ) where

import Math.Growth

-- ANCHOR: double
-- 🏦 Step 2: the first year the money doubles, within 1000 years.
--    Nothing if it never does (0%, or a rate too small to see).
-- ステップ2：お金が2倍になる最初の年（1000 年以内）。
-- 2倍にならなければ Nothing（0%、小さすぎる利率など）
yearsToDouble :: Double -> Maybe Int
yearsToDouble rate =
  case [ y | y <- [0 .. 1000], compound 1 rate y >= 2 ] of
    y : _ -> Just y
    []    -> Nothing
-- ANCHOR_END: double

-- ANCHOR: level
-- ⭐ Step 3: a new player is level 0. Level 1 needs 100 exp, and each
--    level needs twice as much. Exp is Integer, so it never overflows.
-- ステップ3：はじめはレベル0。レベル1は経験値 100、レベルが1上がる
-- ごとに2倍。経験値は、あふれない Integer
expFor :: Int -> Integer
expFor 0     = 0
expFor level = 100 * 2 ^ (level - 1)

-- ⭐ The level for some exp: how many times can exp be halved and
--    still be 100 or more? (The same idea as halvings.)
-- 経験値からレベルへ：半分にしても 100 以上でいられるのは何回？
-- （halvings と同じ考え方）
levelFor :: Integer -> Int
levelFor ex = length (takeWhile (>= 100) (iterate (`div` 2) ex))
-- ANCHOR_END: level
-- ANCHOR_END: all
