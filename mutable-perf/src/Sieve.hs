-- ANCHOR: all
-- | 🔮 The sieve of Eratosthenes: find the prime-numbered floors.
-- エラトステネスのふるい：素数の階を探す
module Sieve
  ( primesList
  , sieveVector
  , primesVector
  , sieveUnsafe
  , sieveArray
  , sieveLoop
  , sieveBits
  ) where

-- ANCHOR: imports
-- Loops, and the ST workshop.
-- ループと、ST の作業場
import Control.Monad    (forM_, when)
import Control.Monad.ST (ST, runST)

-- Vectors, and the array package (used on AtCoder too).
-- Vector と、array パッケージ（AtCoder でもよく使う）
import Data.Array.ST               (newArray, readArray, runSTUArray)
import Data.Array.ST               (writeArray)
import Data.Array.Unboxed          (UArray)
import Data.Bit                    (Bit (..))
import Data.Vector.Unboxed         qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU
-- ANCHOR_END: imports

-- ANCHOR: list
-- 🐢 The "sieve" seen in many introductions: short, but slow.
-- 入門でよく見る「ふるい」。短いけれど、遅い
primesList :: Int -> [Int]
primesList n = sieve [2 .. n]
  where
    sieve []       = []
    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
-- ANCHOR_END: list

-- ANCHOR: vector
-- 🐇 The real sieve: cross out the multiples in a table of Bools.
--    (n must be 1 or more.)
-- 本物のふるい：Bool の表で、倍数に × を付けていく（n は 1 以上）
sieveVector :: Int -> VU.Vector Bool
sieveVector n = runST $ do
  isPrime <- MVU.replicate (n + 1) True
  MVU.write isPrime 0 False
  MVU.write isPrime 1 False
  forM_ (takeWhile (\p -> p * p <= n) [2 ..]) $ \p -> do
    stillPrime <- MVU.read isPrime p
    when stillPrime $
      forM_ [p * p, p * p + p .. n] $ \m ->
        MVU.write isPrime m False
  VU.freeze isPrime

-- 📜 The primes up to n, from the table.
-- 表から、n 以下の素数を取り出す
primesVector :: Int -> [Int]
primesVector n = VU.toList (VU.elemIndices True (sieveVector n))
-- ANCHOR_END: vector

-- ANCHOR: unsafe
-- ⚡ The same sieve without the index checks (we promise the indexes
--    are in range) and without the last copy.
-- 同じふるいを、番号の確かめなしで（範囲内だと約束する）、
-- 最後のコピーもなしで
sieveUnsafe :: Int -> VU.Vector Bool
sieveUnsafe n = runST $ do
  isPrime <- MVU.replicate (n + 1) True
  MVU.unsafeWrite isPrime 0 False
  MVU.unsafeWrite isPrime 1 False
  forM_ (takeWhile (\p -> p * p <= n) [2 ..]) $ \p -> do
    stillPrime <- MVU.unsafeRead isPrime p
    when stillPrime $
      forM_ [p * p, p * p + p .. n] $ \m ->
        MVU.unsafeWrite isPrime m False
  VU.unsafeFreeze isPrime
-- ANCHOR_END: unsafe

-- ANCHOR: array
-- 🧮 The same sieve with the array package's STUArray.
-- 同じふるいを、array パッケージの STUArray で
sieveArray :: Int -> UArray Int Bool
sieveArray n = runSTUArray $ do
  isPrime <- newArray (0, n) True
  writeArray isPrime 0 False
  writeArray isPrime 1 False
  forM_ (takeWhile (\p -> p * p <= n) [2 ..]) $ \p -> do
    stillPrime <- readArray isPrime p
    when stillPrime $
      forM_ [p * p, p * p + p .. n] $ \m ->
        writeArray isPrime m False
  pure isPrime
-- ANCHOR_END: array

-- ANCHOR: loop
-- 🏎️ The same sieve with hand-written loops, as Rust's while does it.
-- 同じふるいを、手で書いたループで（Rust の while と同じ形）
sieveLoop :: Int -> VU.Vector Bool
sieveLoop n = runST $ do
  isPrime <- MVU.replicate (n + 1) True
  MVU.unsafeWrite isPrime 0 False
  MVU.unsafeWrite isPrime 1 False
  eachPrime isPrime n 2
  VU.unsafeFreeze isPrime

-- 🔁 For p = 2, 3, ... while p * p <= n.
-- p = 2, 3, ... と、p * p <= n のあいだ
eachPrime :: MVU.MVector s Bool -> Int -> Int -> ST s ()
eachPrime isPrime n p
  | p * p > n = pure ()
  | otherwise = do
      stillPrime <- MVU.unsafeRead isPrime p
      when stillPrime (crossOut isPrime n p (p * p))
      eachPrime isPrime n (p + 1)

-- ❌ Cross out m, m + p, m + 2p, ... up to n.
-- m, m + p, m + 2p, ... と、n まで × を付ける
crossOut :: MVU.MVector s Bool -> Int -> Int -> Int -> ST s ()
crossOut isPrime n p m
  | m > n     = pure ()
  | otherwise = do
      MVU.unsafeWrite isPrime m False
      crossOut isPrime n p (m + p)
-- ANCHOR_END: loop

-- ANCHOR: bits
-- 🧩 The same sieve with bitvec's Bit: 8 of them packed in one byte.
-- 同じふるいを、bitvec の Bit で。1バイトに8つ詰める
sieveBits :: Int -> VU.Vector Bit
sieveBits n = runST $ do
  isPrime <- MVU.replicate (n + 1) (Bit True)
  MVU.write isPrime 0 (Bit False)
  MVU.write isPrime 1 (Bit False)
  forM_ (takeWhile (\p -> p * p <= n) [2 ..]) $ \p -> do
    Bit stillPrime <- MVU.read isPrime p
    when stillPrime $
      forM_ [p * p, p * p + p .. n] $ \m ->
        MVU.write isPrime m (Bit False)
  VU.freeze isPrime
-- ANCHOR_END: bits
-- ANCHOR_END: all
