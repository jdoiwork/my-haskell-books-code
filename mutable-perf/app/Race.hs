-- ANCHOR: all
-- Without this, GHC computes the sieve once and shares it in all 3 runs.
-- これがないと、GHC はふるいを1回だけ計算して、3回で使い回してしまう
{-# OPTIONS_GHC -fno-full-laziness #-}

-- | The sieve race: how long does a sieve up to n take? (the best of 3)
-- ふるいの競走：n までのふるいに、どれだけかかる？（3回のうちの最速）
--   sieve-race loop 10000000
module Main (main) where

import Control.Exception  (evaluate)
import Control.Monad      (replicateM)
import GHC.Clock          (getMonotonicTime)
import System.Environment (getArgs)
import Text.Printf        (printf)

import Data.Vector.Unboxed qualified as VU

import Sieve (sieveLoop, sieveUnsafe, sieveVector)

-- ⏱️ Sieve once, and return the number of primes and the milliseconds.
-- 1回ふるって、素数の数とかかったミリ秒を返す
timeOnce :: (Int -> VU.Vector Bool) -> Int -> IO (Int, Double)
timeOnce sieve n = do
  start <- getMonotonicTime
  count <- evaluate (VU.length (VU.filter id (sieve n)))
  end   <- getMonotonicTime
  pure (count, (end - start) * 1000)

-- 🏁 The runners.
-- 走る人たち
sieves :: [(String, Int -> VU.Vector Bool)]
sieves =
  [ ("vector", sieveVector)
  , ("unsafe", sieveUnsafe)
  , ("loop"  , sieveLoop)
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [name, arg] | Just sieve <- lookup name sieves -> do
      runs <- replicateM 3 (timeOnce sieve (read arg))
      let counts = map fst runs
          best   = minimum (map snd runs)
      printf "%d %.1f\n" (maximum counts) best
    -- 「使い方：sieve-race vector|unsafe|loop N」
    _ -> putStrLn "usage: sieve-race vector|unsafe|loop N"
-- ANCHOR_END: all
