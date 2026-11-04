-- ANCHOR: all
-- | Slopes and gradients: walking uphill and downhill to an answer.
-- 傾きと勾配：坂を登ったり下ったりして、答えを探す
module Math.Calculus
  ( slope
  , derivative
  , slopeErrors
  , revenue
  , climb
  , height
  , gradient
  , descend
  ) where

-- ANCHOR: slope
-- 📐 The slope between x and x + h: how much f rises per step.
-- x から x + h までの傾き：1歩あたり、f がどれだけ上がるか
slope :: (Double -> Double) -> Double -> Double -> Double
slope f x h = (f (x + h) - f x) / h

-- 📐 The slope at x itself: take h small, and look both ways.
-- x そのものでの傾き：h を小さくして、両側を見る
derivative :: (Double -> Double) -> Double -> Double
derivative f x = (f (x + h) - f (x - h)) / (2 * h)
  where
    h = 1e-5
-- ANCHOR_END: slope

-- ANCHOR: errors
-- 🔬 How far off is the slope of x^2 at 3 (truly 6), for smaller and
--    smaller steps h = 10^-1, 10^-2, ..., 10^-15?
-- x^2 の 3 での傾き（本当は 6）は、h を小さくしていくと、どれだけずれる？
slopeErrors :: [(Int, Double)]
slopeErrors =
  [ (k, abs (slope (\x -> x * x) 3 (10 ** fromIntegral (negate k)) - 6))
  | k <- [1 .. 15] ]
-- ANCHOR_END: errors

-- ANCHOR: climb
-- 🏪 The shop sells fewer potions when the price p goes up:
--    100 - 2p of them. The money it takes: p * (100 - 2p).
-- 値段 p を上げると、売れる数は 100 - 2p に減る。売上は p * (100 - 2p)
revenue :: Double -> Double
revenue p = p * (100 - 2 * p)

-- ⛰️ Walk uphill: move in the direction the slope points, a little
--    (rate * slope) at a time. The prices along the way.
-- 坂を登る：傾きの向きへ、少しずつ（学習率 × 傾き）進む。途中の値段のリスト
climb :: (Double -> Double) -> Double -> Double -> [Double]
climb f rate start = iterate step start
  where
    step x = x + rate * derivative f x
-- ANCHOR_END: climb

-- ANCHOR: descend
-- 🕳️ The ground of a valley: the lowest point is at (3, -1).
--    It is steeper north-south (the 2) than east-west.
-- くぼ地の地面：いちばん低い点は (3, -1)。南北（2 の向き）のほうが急
height :: (Double, Double) -> Double
height (x, y) = (x - 3) ^ (2 :: Int) + 2 * (y + 1) ^ (2 :: Int)

-- 🧭 The gradient: the slope in each direction, as a vector.
--    It points the way the ground rises fastest.
-- 勾配：それぞれの向きの傾きを並べたベクトル。いちばん急に上る向きを指す
gradient :: ((Double, Double) -> Double) -> (Double, Double)
         -> (Double, Double)
gradient f (x, y) =
  ( derivative (\x' -> f (x', y)) x
  , derivative (\y' -> f (x, y')) y )

-- ⬇️ Gradient descent: step against the gradient, again and again.
-- 勾配降下法：勾配の逆へ、くり返し進む
descend :: Double -> (Double, Double) -> [(Double, Double)]
descend rate start = iterate step start
  where
    step (x, y) =
      let (gx, gy) = gradient height (x, y)
      in  (x - rate * gx, y - rate * gy)
-- ANCHOR_END: descend
-- ANCHOR_END: all
