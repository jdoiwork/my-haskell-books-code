-- A first try of hasktorch: multiply two matrices with libtorch, and time it.
-- hasktorch を試す：libtorch で行列を掛けて、時間を測る
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad     (forM, forM_)
import Data.List         (sort)
import GHC.Clock         (getMonotonicTime)
import Text.Printf       (printf)
import Torch

-- ⏱️ Multiply once, and force the answer (asValue reads every number).
-- 1回掛けて、答えを最後まで計算させる（asValue が全部の数を読む）
timeOnce :: Tensor -> Tensor -> IO Double
timeOnce a b = do
  t0 <- getMonotonicTime
  _  <- evaluate (asValue (sumAll (matmul a b)) :: Double)
  t1 <- getMonotonicTime
  pure (t1 - t0)

main :: IO ()
main = do
  -- The small check first. / まず小さく確かめる
  let a = asTensor ([[1, 2], [3, 4]] :: [[Double]])
      b = asTensor ([[5, 6], [7, 8]] :: [[Double]])
  print (asValue (matmul a b) :: [[Double]])
  forM_ [256, 512, 1024 :: Int] $ \n -> do
    x <- randIO' [n, n]
    y <- randIO' [n, n]
    let x' = toDType Double x
        y' = toDType Double y
    _  <- timeOnce x' y'
    ts <- forM [1 .. 10 :: Int] (\_ -> timeOnce x' y')
    printf "hasktorch n=%d median=%.2f ms\n" n (sort ts !! 5 * 1000)
