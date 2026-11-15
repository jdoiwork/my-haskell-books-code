-- ANCHOR: head
-- ANCHOR: all
-- | Using autograd: the same function, three ways, and a regression.
-- autograd を使う：同じ関数を3通りに、それから回帰。
module AutogradDemo
  ( f
  , fitWithAutograd
  ) where

import Autograd
import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
-- ANCHOR_END: head

-- ANCHOR: f
-- 🧮 One function, written for ANY kind of number (Floating a).
--    It works with Double, with Dual, and with Value.
-- どんな種類の数でも使える関数（Floating a）。
-- Double でも、Dual でも、Value でも使える
f :: Floating a => a -> a -> a
f x y = x * y + sin x
-- ANCHOR_END: f

-- ANCHOR: fit
-- 🏋️ Linear regression again, but the gradients come from autograd.
--    We only write the loss; backward finds d(loss)/dw and d(loss)/db.
-- もう一度、線形回帰。ただし傾きは autograd が求める。
-- 書くのは損失だけ。backward が d（損失）/dw と d（損失）/db を見つける
fitWithAutograd :: Double -> Int -> NDArray -> NDArray -> (Double, Double)
fitWithAutograd lr steps x y = go steps (0, 0)
  where
    pairs = zip (Np.toList x) (Np.toList y)
    n     = fromIntegral (length pairs)
    go 0 wb = wb
    go k (w0, b0) =
      let w     = param "w" w0
          b     = param "b" b0
          loss  = sum [ (w * constant xi + b - constant yi) ^ (2 :: Int)
                      | (xi, yi) <- pairs ] / constant n
          grads = backward loss
          -- MSE has a 2 in its gradient; halve it to match chapter 2.
          -- MSE の傾きには 2 が付くので、前の章に合わせて半分にする
          w1    = w0 - lr * grad "w" grads / 2
          b1    = b0 - lr * grad "b" grads / 2
      in go (k - 1) (w1, b1)
-- ANCHOR_END: fit
-- ANCHOR_END: all
