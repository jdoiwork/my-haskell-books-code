-- ANCHOR: all
-- ANCHOR: head
-- 🏋️ Answers for the training.
-- トレーニングの解答
module CompeteAnswers (countAtLeast, waysToPay, waysToPayNaive) where

import Data.List           qualified as L
import Data.Vector.Unboxed qualified as VU
-- ANCHOR_END: head

-- ANCHOR: count
-- 🔢 How many prices in a sorted vector are at least x: binary search.
-- 小さい順に並んだ値段のうち、x 以上のものの数：二分探索
countAtLeast :: VU.Vector Int -> Int -> Int
countAtLeast prices x = VU.length prices - firstAtLeast 0 (VU.length prices)
  where
    -- the first position whose price is at least x, in [lo, hi]
    -- 値段が x 以上になる最初の位置（lo から hi の間で）
    firstAtLeast lo hi
      | lo >= hi               = lo
      | prices VU.! mid >= x  = firstAtLeast lo mid
      | otherwise              = firstAtLeast (mid + 1) hi
      where
        mid = (lo + hi) `div` 2
-- ANCHOR_END: count

-- ANCHOR: ways
-- 🪙 How many ways to pay exactly this amount with these coins
--    (each kind as many times as we like): dynamic programming.
-- この金額ちょうどを、これらの硬貨で払う方法の数
-- （どの硬貨も何枚でも使える）：動的計画法
waysToPay :: [Int] -> Int -> Int
waysToPay coins amount = L.foldl' addCoin start coins VU.! amount
  where
    start = VU.generate (amount + 1) (\a -> if a == 0 then 1 else 0)
    addCoin ways coin = VU.constructN (amount + 1) step
      where
        step sofar
          | a < coin  = ways VU.! a
          | otherwise = ways VU.! a + sofar VU.! (a - coin)
          where
            a = VU.length sofar

-- 🐢 The same, by trying every choice (slow, but clearly right).
-- 同じものを、すべての選び方を試して（遅いが、正しいとわかりやすい）
waysToPayNaive :: [Int] -> Int -> Int
waysToPayNaive _ 0 = 1
waysToPayNaive [] _ = 0
waysToPayNaive (c : cs) amount
  | amount < 0 = 0
  | otherwise  = waysToPayNaive (c : cs) (amount - c)
                   + waysToPayNaive cs amount
-- ANCHOR_END: ways
-- ANCHOR_END: all
