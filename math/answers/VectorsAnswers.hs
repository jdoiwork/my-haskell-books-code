-- ANCHOR: all
-- | Answers to the training of "Vectors and matrices".
-- 「ベクトルと行列」のトレーニングの解答
module VectorsAnswers
  ( distance
  , nearest
  , fibByMatrix
  ) where

import Data.List (sortOn)

import Math.Vectors

-- ANCHOR: distance
-- 📏 Step 2: how far apart two points are.
-- ステップ2：2つの点の距離
distance :: Vector Double -> Vector Double -> Double
distance u v = norm (add u (scale (-1) v))
-- ANCHOR_END: distance

-- ANCHOR: nearest
-- 📋 The other monsters, nearest first (by distance).
-- ほかのモンスターを、距離の近い順に
nearest :: String -> [String]
nearest name = case lookup name monsters of
  Nothing -> []
  Just me ->
    let others = [ m | m@(n, _) <- monsters, n /= name ]
    in  map fst (sortOn (distance me . snd) others)
-- ANCHOR_END: nearest

-- ANCHOR: fib
-- 🐇 Step 3: Fibonacci with a matrix: [[1,1],[1,0]] ^ n holds F(n)
--    (n is 0 or more). Squaring as in the fast power: at most about
--    2 log2 n matrix products.
-- ステップ3：行列でフィボナッチ。[[1,1],[1,0]] の n 乗に F(n) が入る
-- （n は 0 以上）。速いべき乗と同じく2乗をくり返すので、行列の積は
-- 多くても約 2 log2 n 回
fibByMatrix :: Int -> Integer
fibByMatrix n
  | n < 0     = error "fibByMatrix: n must be 0 or more"
  | otherwise = case power [[1, 1], [1, 0]] n of
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
