-- ANCHOR: all
-- | Answers to the training of "Vectors and matrices".
-- 「ベクトルと行列」のトレーニングの解答
module VectorsAnswers
  ( distance
  , fibByMatrix
  ) where

import Math.Vectors

-- ANCHOR: distance
-- 📏 Step 2: how far apart two points are.
-- ステップ2：2つの点の距離
distance :: Vector Double -> Vector Double -> Double
distance u v = norm (add u (scale (-1) v))
-- ANCHOR_END: distance

-- ANCHOR: fib
-- 🐇 Step 3: Fibonacci with a matrix: [[1,1],[1,0]] ^ n holds F(n).
--    Squaring as in the fast power: about log2 n matrix products.
-- ステップ3：行列でフィボナッチ。[[1,1],[1,0]] の n 乗に F(n) が入る。
-- 速いべき乗と同じく2乗をくり返すので、行列の積はだいたい log2 n 回
fibByMatrix :: Int -> Integer
fibByMatrix n = case power [[1, 1], [1, 0]] n of
  [_, [f, _]] -> f
  _           -> 0
  where
    power :: Matrix Integer -> Int -> Matrix Integer
    power m k
      | k == 0    = identity 2
      | even k    = let half = power m (k `div` 2) in mmul half half
      | otherwise = mmul m (power m (k - 1))
-- ANCHOR_END: fib
-- ANCHOR_END: all
