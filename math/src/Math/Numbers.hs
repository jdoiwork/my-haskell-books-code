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
  , fastInvSqrt
  ) where

-- ANCHOR: imports
import Data.Bits (setBit, shiftR, testBit)
import GHC.Float (castFloatToWord32, castWord32ToFloat)
-- ANCHOR_END: imports

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

-- ANCHOR: fastInvSqrt
-- 🪄 1 / sqrt x, from the bits of a Float and a magic number
--    (the famous trick from the source code of Quake III Arena).
-- Float のビットと魔法の数で 1 / sqrt x を求める
-- （Quake III Arena のソースコードで有名になった技）
fastInvSqrt :: Float -> Float
fastInvSqrt x =
  let -- Read the bits of the Float as a whole number.
      -- Float のビットを、そのまま整数として読む
      bits  = castFloatToWord32 x
      -- The magic number minus half of the bits: a rough guess.
      -- 魔法の数から、ビットの半分を引く（大まかな当て推量）
      guess = castWord32ToFloat (0x5f3759df - bits `shiftR` 1)
  in  -- One step of Newton's method makes it much closer.
      -- ニュートン法で1回だけ磨くと、ぐっと近くなる
      guess * (1.5 - 0.5 * x * guess * guess)
-- ANCHOR_END: fastInvSqrt
-- ANCHOR_END: all
