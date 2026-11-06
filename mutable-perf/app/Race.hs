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

import Data.Bit (countBits)

import Sieve (sieveBits, sieveLoop, sieveUnsafe, sieveVector)

-- ⏱️ Sieve once, and return the number of primes and the milliseconds.
-- 1回ふるって、素数の数とかかったミリ秒を返す
timeOnce :: (Int -> Int) -> Int -> IO (Int, Double)
timeOnce countPrimes n = do
  start <- getMonotonicTime
  count <- evaluate (countPrimes n)
  end   <- getMonotonicTime
  pure (count, (end - start) * 1000)

-- ✅ Count the Trues.
-- True の数を数える
countTrue :: VU.Vector Bool -> Int
countTrue v = VU.length (VU.filter id v)

-- 🏁 The runners.
-- 走る人たち
sieves :: [(String, Int -> Int)]
sieves =
  [ ("vector", \n -> countTrue (sieveVector n))
  , ("unsafe", \n -> countTrue (sieveUnsafe n))
  , ("loop"  , \n -> countTrue (sieveLoop n))
  , ("bits"  , \n -> countBits (sieveBits n))
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [name, arg] | Just countPrimes <- lookup name sieves -> do
      runs <- replicateM 3 (timeOnce countPrimes (read arg))
      let counts = map fst runs
          best   = minimum (map snd runs)
      printf "%d %.1f\n" (maximum counts) best
    -- 「使い方：sieve-race vector|unsafe|loop|bits N」
    _ -> putStrLn "usage: sieve-race vector|unsafe|loop|bits N"
-- ANCHOR_END: all
