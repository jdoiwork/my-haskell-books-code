-- ANCHOR: all
-- | Answers to the training of "Derivatives and gradients".
-- 「微分と勾配」のトレーニングの解答
module CalculusAnswers
  ( newton
  , bestAngle
  ) where

import Math.Calculus

-- ANCHOR: newton
-- 🎯 Step 2: Newton's method for f(x) = 0: follow the tangent line
--    down to where it crosses 0, and start again from there.
-- ステップ2：f(x) = 0 を解くニュートン法：接線を 0 まで伸ばし、
-- そこからまた始める
newton :: (Double -> Double) -> Double -> [Double]
newton f start = iterate step start
  where
    step x = x - f x / derivative f x
-- ANCHOR_END: newton

-- ANCHOR: angle
-- 🏹 Step 3: the angle (in degrees) that throws a stone the farthest.
--    The distance is proportional to sin (2 * angle).
-- ステップ3：石がいちばん遠くへ飛ぶ角度（度）。距離は sin (2 × 角度) に比例
bestAngle :: Double
bestAngle = climb range 100 10 !! 200
  where
    range deg = sin (2 * deg * pi / 180)
-- ANCHOR_END: angle
-- ANCHOR_END: all
