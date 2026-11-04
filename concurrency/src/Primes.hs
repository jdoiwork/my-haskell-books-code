-- ANCHOR: all
-- 🔢 Counting primes: hard work for the CPU, split into parts.
-- 素数を数える：CPU の重い仕事を、いくつかに分ける
module Primes (isPrime, countPrimes, countPrimesParallel) where

import Control.Parallel.Strategies (parMap, rdeepseq)

-- ANCHOR: primes
-- 🔢 Is n a prime? Try dividing by 2, 3, 4, ... up to its square root.
-- n は素数？ 2, 3, 4, … と、平方根まで割ってみる
isPrime :: Int -> Bool
isPrime n
  | n < 2     = False
  | otherwise = all (\d -> n `mod` d /= 0) candidates
  where
    candidates = takeWhile (\d -> d * d <= n) [2 ..]

-- 🔢 How many primes are there from 1 to n?
-- 1 から n までに、素数はいくつ？
countPrimes :: Int -> Int
countPrimes n = length (filter isPrime [1 .. n])

-- ⚡ The same, in 8 parts that run in parallel.
-- 同じものを、8つに分けて並列に
countPrimesParallel :: Int -> Int
countPrimesParallel n = sum (parMap rdeepseq countIn parts)
  where
    size  = n `div` 8 + 1
    parts = [ (lo, min n (lo + size - 1)) | lo <- [1, 1 + size .. n] ]
    countIn (lo, hi) = length (filter isPrime [lo .. hi])
-- ANCHOR_END: primes
-- ANCHOR_END: all
