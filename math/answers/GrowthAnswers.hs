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
-- 🏦 Step 2: how many years until the money doubles?
-- ステップ2：お金が2倍になるのは何年後？
yearsToDouble :: Double -> Int
yearsToDouble rate =
  length (takeWhile (< 2) [ compound 1 rate y | y <- [0 ..] ])
-- ANCHOR_END: double

-- ANCHOR: level
-- ⭐ Step 3: level 1 needs 100 exp, and each level needs twice as much.
-- ステップ3：レベル1は経験値 100、レベルが1上がるごとに2倍
expFor :: Int -> Int
expFor level = 100 * 2 ^ (level - 1)

-- ⭐ The level for some exp: how many times can exp / 100 be halved?
-- 経験値からレベルへ：exp / 100 を何回半分にできる？
levelFor :: Int -> Int
levelFor ex = 1 + halvings (ex `div` 100)
-- ANCHOR_END: level
-- ANCHOR_END: all
