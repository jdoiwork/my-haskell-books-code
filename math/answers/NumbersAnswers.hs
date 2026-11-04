-- ANCHOR: all
-- | Answers to the training of "What numbers really are".
-- 「数の正体」のトレーニングの解答
module NumbersAnswers
  ( removeStatus
  , statusNames
  , mulGold
  ) where

import Data.Bits (clearBit)

import Math.Numbers

-- ANCHOR: remove
-- 💊 Step 2: cure one status: turn its bit off.
-- ステップ2：状態異常を1つ治す。そのビットを下ろす
removeStatus :: Int -> Status -> Status
removeStatus which s = clearBit s which

-- 📋 Step 2: the names of all the statuses that are on.
-- ステップ2：立っている状態異常の名前をすべて
statusNames :: Status -> [String]
statusNames s =
  [ name
  | (which, name) <- names
  , hasStatus which s
  ]
  where
    names = [(poison, "poison"), (sleep, "sleep"), (silence, "silence")]
-- ANCHOR_END: remove

-- ANCHOR: mul
-- ✖️ Step 3: multiply gold, but Nothing when it would overflow.
--    Compute in Integer (which never overflows), then check the range.
-- ステップ3：ゴールドを掛ける。あふれるときは Nothing。
-- あふれない Integer で計算してから、範囲に入っているかを見る
mulGold :: Int -> Int -> Maybe Int
mulGold a b =
  let exact = toInteger a * toInteger b
  in  if exact > toInteger (maxBound :: Int)
         || exact < toInteger (minBound :: Int)
        then Nothing
        else Just (fromInteger exact)
-- ANCHOR_END: mul
-- ANCHOR_END: all
