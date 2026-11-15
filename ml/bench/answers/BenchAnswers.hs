-- ANCHOR: all
-- ANCHOR: head
-- | Answers to the training of the benchmark chapter.
-- 速さを比べる章のトレーニングの解答
module BenchAnswers
  ( timeForgetful
  , dotMV
  , softmaxRowsST
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import GHC.Clock (getMonotonicTime)
import qualified Numeric.LinearAlgebra as LA

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
import qualified MiniNumpy.Blas as Blas
-- ANCHOR_END: head

-- ANCHOR: forgetful
-- ⏱️ Step 1: a stopwatch that forgets to compute. `pure (f x)` hands
--    back the work undone (a thunk), so the clock stops before the work.
-- ステップ1：計算させ忘れたストップウォッチ。`pure (f x)` は、まだ計算して
-- いない仕事（サンク）を返すだけなので、仕事の前に時計が止まる
timeForgetful :: (a -> b) -> a -> IO Double
timeForgetful f x = do
  t0 <- getMonotonicTime
  _  <- pure (f x)
  t1 <- getMonotonicTime
  pure (t1 - t0)
{-# NOINLINE timeForgetful #-}
-- ANCHOR_END: forgetful

-- ANCHOR: dotMV
-- ✖️ Step 2: a matrix times a vector, also by BLAS (hmatrix's #>).
-- ステップ2：行列×ベクトルも BLAS で（hmatrix の #>）
dotMV :: NDArray -> NDArray -> NDArray
dotMV a v = case (Np.shape a, Np.shape v) of
  ([r, k], [k']) | k == k' && r > 0 && k > 0 ->
    let y = Blas.toMatrix a LA.#> VU.convert (Np.values v)
    in Np.fromVector [LA.size y] (VU.convert y)
  _ -> Blas.dot a v
-- ANCHOR_END: dotMV

-- ANCHOR: softmaxST
-- ✍️ Step 3: softmax of each row, written into one new vector in place.
--    exp is computed once per number, and no array is made in between.
-- ステップ3：行ごとの softmax を、新しい Vector 1つに書きこんで作る。
-- exp は1つの数に1回だけ。途中の配列を作らない
softmaxRowsST :: NDArray -> NDArray
softmaxRowsST x = case Np.shape x of
  [r, c] -> Np.fromVector [r, c] (VU.create (fill r c (Np.values x)))
  s      -> error ("softmaxRowsST: not 2-D: " <> show s)

fill :: Int -> Int -> VU.Vector Double -> ST s (MVU.MVector s Double)
fill r c xs = do
  out <- MVU.new (r * c)
  forM_ [0 .. r - 1] $ \i -> do
    let row = VU.slice (i * c) c xs
        m   = VU.maximum row
        go !j !s
          | j == c    = pure s
          | otherwise = do
              let e = exp (row VU.! j - m)
              MVU.write out (i * c + j) e
              go (j + 1) (s + e)
    total <- go 0 0
    forM_ [0 .. c - 1] $ \j ->
      MVU.modify out (/ total) (i * c + j)
  pure out
-- ANCHOR_END: softmaxST
-- ANCHOR_END: all
