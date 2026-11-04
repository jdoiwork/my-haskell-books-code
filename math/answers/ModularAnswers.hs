-- ANCHOR: all
-- | Answers to the training of "The world of remainders".
-- 「剰余の世界」のトレーニングの解答
module ModularAnswers
  ( Weekday (..)
  , weekdayAfter
  , isbnCheckDigit
  ) where

-- ANCHOR: weekday
-- 📅 Step 2: the weekday n days after a weekday.
-- ステップ2：ある曜日の n 日後の曜日
data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show, Eq, Enum, Bounded)

weekdayAfter :: Weekday -> Int -> Weekday
weekdayAfter day n = toEnum ((fromEnum day + n) `mod` 7)
-- ANCHOR_END: weekday

-- ANCHOR: isbn
-- 📚 Step 3: the last digit of an ISBN-13 from the first 12 digits:
--    weights 1, 3, 1, 3, ...; the total must be a multiple of 10.
-- ステップ3：ISBN-13 の最後の数字。重みは 1, 3, 1, 3, ... で、
-- 合計が 10 の倍数になるように選ぶ
isbnCheckDigit :: [Int] -> Int
isbnCheckDigit digits =
  let total = sum (zipWith (*) (cycle [1, 3]) digits)
  in  (10 - total `mod` 10) `mod` 10
-- ANCHOR_END: isbn
-- ANCHOR_END: all
