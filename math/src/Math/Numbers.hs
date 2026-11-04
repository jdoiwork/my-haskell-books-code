-- ANCHOR: all
-- | The numbers of the item shop: digits, overflow, bits, and money.
-- 道具屋の数：桁、あふれ、ビット、お金
module Math.Numbers
  ( toDigits
  , fromDigits
  , addGold
  , Status
  , poison
  , sleep
  , silence
  , noStatus
  , addStatus
  , hasStatus
  , withTax
  ) where

import Data.Bits (setBit, testBit)

-- ANCHOR: digits
-- 🔢 The digits of n in a base, from the top: toDigits 2 6 == [1, 1, 0].
--    Divide by the base again and again; the remainders are the digits.
-- n を base 進数で書いたときの桁（上の桁から）。
-- base で割り続けると、余りが桁になる
toDigits :: Int -> Int -> [Int]
toDigits base n
  | n < base  = [n]
  | otherwise =
      let (rest, digit) = n `divMod` base
      in  toDigits base rest ++ [digit]

-- 🔢 Back from digits: fromDigits 2 [1, 1, 0] == 6.
-- 桁から数に戻す
fromDigits :: Int -> [Int] -> Int
fromDigits base digits = go 0 digits
  where
    go acc []       = acc
    go acc (d : ds) = go (acc * base + d) ds
-- ANCHOR_END: digits

-- ANCHOR: addGold
-- 💰 Add gold, but say Nothing instead of going round to a minus number.
--    (Like checked_add in Rust.)
-- ゴールドを足す。マイナスに一周してしまうときは Nothing
addGold :: Int -> Int -> Maybe Int
addGold a b
  | b > 0 && a > maxBound - b = Nothing
  | b < 0 && a < minBound - b = Nothing
  | otherwise                 = Just (a + b)
-- ANCHOR_END: addGold

-- ANCHOR: status
-- 🧪 Status effects as bits of one Int: bit 0 is poison, bit 1 is sleep,
--    bit 2 is silence. 0b101 means "poisoned and silenced".
-- 状態異常を、1つの Int のビットで表す。
-- 0 番のビットが毒、1 番が眠り、2 番が沈黙
type Status = Int

poison, sleep, silence :: Int
poison  = 0
sleep   = 1
silence = 2

noStatus :: Status
noStatus = 0

-- ➕ Turn one bit on. / ビットを1つ立てる
addStatus :: Int -> Status -> Status
addStatus which s = setBit s which

-- ❓ Is that bit on? / そのビットは立っている？
hasStatus :: Int -> Status -> Bool
hasStatus which s = testBit s which
-- ANCHOR_END: status

-- ANCHOR: money
-- 🧾 Add 10% tax to a price in cents, rounding half up, with Int only:
--    withTax 105 == 116 (115.5 rounds up).
-- 値段（セント）に 10% の税を足し、四捨五入する。Int だけで計算する
withTax :: Int -> Int
withTax cents = (cents * 110 + 50) `div` 100
-- ANCHOR_END: money
-- ANCHOR_END: all
