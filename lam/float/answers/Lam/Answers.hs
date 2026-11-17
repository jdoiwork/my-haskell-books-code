-- ANCHOR: all
-- | The answers of the training of "Adding floats: number in the
--   style of Elm".
-- 「小数を足す：Elm 風の number」のトレーニングの解答。
module Lam.Answers
  ( roundTrips
  ) where

import Lam.Float (readFloat)
import Lam.Pretty (showFloat)

-- ANCHOR: roundTrips
-- 🔁 Step 4: a Float shown with showFloat is read back by readFloat
--    (String.toFloat) as the very same Float. NaN and the infinities
--    are left out: they are not numbers to write.
-- ステップ4：showFloat で見せた Float は、readFloat（String.toFloat）で
-- 読むと、まったく同じ Float に戻る。NaN と無限大は外す（書ける数ではない）
roundTrips :: Double -> Bool
roundTrips x
  | isNaN x || isInfinite x = True
  | otherwise               = readFloat (showFloat x) == Just x
-- ANCHOR_END: roundTrips
-- ANCHOR_END: all
