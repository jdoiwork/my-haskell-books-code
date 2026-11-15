-- ANCHOR: all
-- ANCHOR: head
-- | The race: the same calculations in several ways, timed on one machine.
--   cabal run bench-ml -- matmul      (or softmax, or nothing for both)
--   For massiv on all the cores:  cabal run bench-ml -- softmax +RTS -N
-- 比べっこ：同じ計算を、いくつかの書き方で、同じ機械で測る
module Main (main) where

import Control.Monad (forM_, when)
import System.Environment (getArgs)
import Text.Printf (printf)

import qualified Data.Massiv.Array as A
import qualified Numeric.LinearAlgebra as LA

import qualified MiniNumpy as Np
import qualified MiniNumpy.Blas as Blas
import Bench.Kernels
import Bench.Timer
import BenchAnswers
-- ANCHOR_END: head

-- ANCHOR: report
-- 🖨️ One line of the table: what, how big, how, and the median.
-- 表の1行：何を、どの大きさで、どの書き方で、中央値
report :: String -> Int -> String -> Double -> IO ()
report task n how ms = printf "%-8s n=%-5d %-17s %10.2f ms\n" task n how ms
-- ANCHOR_END: report

-- ANCHOR: matmul
-- ✖️ The matrix product of two n × n matrices, three ways.
--    The three loops are slow, so only for the smaller sizes.
-- n × n の行列の積を、3つの書き方で。3重ループは遅いので、小さいほうだけ
matmulRace :: IO ()
matmulRace =
  forM_ [256, 512, 1024] $ \n -> do
    let a = randomMatrix 1 n n
        b = randomMatrix 2 n n
    when (n <= 512) $
      report "matmul" n "naive"
        =<< medianMs 3 (uncurry naiveMatmul) (a, b) checksum
    report "matmul" n "MiniNumpy"
      =<< medianMs 5 (uncurry Np.dot) (a, b) checksum
    report "matmul" n "hmatrix"
      =<< medianMs 10 (uncurry Blas.dot) (a, b) checksum
    -- The same product on matrices that are already hmatrix's:
    -- no copying through the gate.
    -- 同じ積を、はじめから hmatrix の行列で。門を通る写しがない
    let am = Blas.toMatrix a
        bm = Blas.toMatrix b
    report "matmul" n "hmatrix (no copy)"
      =<< medianMs 10 (uncurry (LA.<>)) (am, bm) LA.sumElements
-- ANCHOR_END: matmul

-- ANCHOR: softmax
-- 🧮 Softmax of each row of a 1000 × 1000 matrix, four ways.
-- 1000 × 1000 の行列の、行ごとの softmax を、4つの書き方で
softmaxRace :: IO ()
softmaxRace = do
  let n = 1000
      x = randomMatrix 3 n n
  report "softmax" n "MiniNumpy"
    =<< medianMs 5 softmaxRowsNp x checksum
  report "softmax" n "vector"
    =<< medianMs 10 softmaxRowsV x checksum
  report "softmax" n "massiv Seq"
    =<< medianMs 10 (softmaxRowsMassiv A.Seq) x checksum
  report "softmax" n "massiv Par"
    =<< medianMs 10 (softmaxRowsMassiv A.Par) x checksum
-- ANCHOR_END: softmax

-- ANCHOR: answers
-- 🏋️ The training: a forgetful stopwatch, and the softmax written in ST.
-- トレーニング：計算させ忘れたストップウォッチと、ST で書いた softmax
answersRace :: IO ()
answersRace = do
  let a = randomMatrix 1 256 256
      b = randomMatrix 2 256 256
  forgetful <- timeForgetful (uncurry naiveMatmul) (a, b)
  report "forgetful" 256 "naive" (forgetful * 1000)
  let x = randomMatrix 3 1000 1000
  report "softmax" 1000 "vector" =<< medianMs 10 softmaxRowsV x checksum
  report "softmax" 1000 "ST" =<< medianMs 10 softmaxRowsST x checksum
-- ANCHOR_END: answers

-- ANCHOR: main
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["matmul"]  -> matmulRace
    ["softmax"] -> softmaxRace
    ["answers"] -> answersRace
    _           -> matmulRace >> softmaxRace
-- ANCHOR_END: main
-- ANCHOR_END: all
