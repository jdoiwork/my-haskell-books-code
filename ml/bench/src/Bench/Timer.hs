-- ANCHOR: all
-- | A small stopwatch for benchmarks: warm up once, then take the median.
-- ベンチマーク用の小さなストップウォッチ：1回温めてから、中央値を取る
module Bench.Timer
  ( timeOnce
  , medianMs
  ) where

import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.List (sort)
import GHC.Clock (getMonotonicTime)

-- ANCHOR: once
-- ⏱️ Run f on x once, and the seconds it took. `done` turns the answer
--    into one number, and evaluate computes that number before the clock
--    stops: without it, the lazy answer would be computed later, outside
--    the clock. NOINLINE keeps GHC from computing `f x` only once, outside
--    the loop, and reusing it.
-- f を x に1回使って、かかった秒数を返す。`done` は答えを1つの数にする。
-- evaluate で、時計を止める前にその数を計算させる（しないと、遅延評価の
-- 答えは時計の外で計算される）。NOINLINE は、GHC が `f x` を
-- ループの外で1回だけ計算して使い回すのを防ぐ
timeOnce :: (a -> b) -> a -> (b -> Double) -> IO Double
timeOnce f x done = do
  t0 <- getMonotonicTime
  _  <- evaluate (done (f x))
  t1 <- getMonotonicTime
  pure (t1 - t0)
{-# NOINLINE timeOnce #-}
-- ANCHOR_END: once

-- ANCHOR: median
-- 📊 Warm up once, run n times, and give the median in milliseconds.
--    The median is not pulled by one slow run (when the machine was busy).
-- 1回温めてから n 回動かし、中央値をミリ秒で返す。
-- 中央値は、たまたま遅かった1回（機械が忙しかったとき）に引っぱられない
medianMs :: Int -> (a -> b) -> a -> (b -> Double) -> IO Double
medianMs n f x done = do
  _  <- timeOnce f x done
  ts <- replicateM n (timeOnce f x done)
  pure (sort ts !! (n `div` 2) * 1000)
-- ANCHOR_END: median
-- ANCHOR_END: all
