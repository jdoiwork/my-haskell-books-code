-- ANCHOR: all
-- | Step 2: stars by tenths (1.0, 1.1, ... 5.0).
-- ステップ2：0.1刻みの★（1.0, 1.1, ... 5.0）
-- ANCHOR: head
module ShelfAnswers.TenthStars
  ( TenthStars
  , tenthStars
  , tenthStarsValue
  , tenthToPoints
  , pointsToTenth
  ) where

import Data.Ord (clamp)

import Tsundoku.Rating (Score, scoreValue)
import Tsundoku.Types (Mistake (..))
-- ANCHOR_END: head

-- ANCHOR: tenth
-- ⭐ Kept as the number of tenths (10 to 50).
-- 0.1 の★がいくつかで持つ（10〜50）
newtype TenthStars = TenthStars { tenths :: Int }
  deriving (Show, Eq, Ord)

-- 🚪 The same check as `stars`, ten times finer. Every tenth from
--    1.0 to 5.0, times 10, lands exactly on a whole number in Double
--    (the test tries all 41), so an exact check is fine here.
-- stars と同じ確かめ方を、10倍細かく。1.0〜5.0 の 0.1 刻みは、
-- Double でも 10倍するとちょうど整数になる（テストで41通りを試す）ので、
-- ぴったりで比べてよい
tenthStars :: Double -> Either Mistake TenthStars
tenthStars x
  | x < 1 || x > 5               = Left (BadStars x)
  | scaled /= fromIntegral whole = Left (BadStars x)
  | otherwise                    = Right (TenthStars whole)
  where
    scaled = x * 10
    whole  = round scaled :: Int

tenthStarsValue :: TenthStars -> Double
tenthStarsValue (TenthStars t) = fromIntegral t / 10

-- 🔁 1.0 star is 20 points, 5.0 stars are 100: 2 points a tenth.
--    (`Score` has no constructor outside its module, so we give an Int.)
-- ★1.0 が 20 点、★5.0 が 100 点。0.1 で 2 点。
-- （Score のコンストラクターはモジュールの外に出ていないので、Int で返す）
tenthToPoints :: TenthStars -> Int
tenthToPoints (TenthStars t) = t * 2

-- 🔁 Points to the nearest tenth, halves up (25 points is 1.3 stars).
-- 点を、いちばん近い 0.1 の★へ。真ん中は切り上げ（25 点は★1.3）
pointsToTenth :: Score -> TenthStars
pointsToTenth p = TenthStars (clamp (10, 50) ((scoreValue p + 1) `div` 2))
-- ANCHOR_END: tenth
-- ANCHOR_END: all
