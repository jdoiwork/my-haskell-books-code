-- ANCHOR: all
-- | Answers to the training of "Induction and recursion".
-- 「帰納法と再帰」のトレーニングの解答
module InductionAnswers
  ( sumOdds
  , deepest
  ) where

import Math.Induction

-- ANCHOR: odds
-- 🔢 Step 2: 1 + 3 + 5 + ... (the first n odd numbers).
-- ステップ2：最初の n 個の奇数の和
sumOdds :: Integer -> Integer
sumOdds 0 = 0
sumOdds n = (2 * n - 1) + sumOdds (n - 1)
-- ANCHOR_END: odds

-- ANCHOR: deepest
-- 🕳️ Step 3: the most forks on one path from the entrance.
-- ステップ3：入口から1本の道で通る、分かれ道のいちばん多い数
deepest :: Room -> Int
deepest DeadEnd    = 0
deepest (Fork l r) = 1 + max (deepest l) (deepest r)
-- ANCHOR_END: deepest
-- ANCHOR_END: all
