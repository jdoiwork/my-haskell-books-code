-- ANCHOR: all
-- | Forward mode: carry the derivative along with the value
--   (dual numbers).
-- フォワードモード：値と一緒に、導関数も運ぶ（二重数）。
module Dual
  ( Dual (..)
  , derivative
  ) where

-- ANCHOR: dual
-- 👯 A value, and how fast it changes when x changes.
-- 値と、x を動かしたときに、それがどれだけ速く変わるか
data Dual = Dual Double Double
  deriving (Show)

instance Num Dual where
  Dual a a' + Dual b b' = Dual (a + b) (a' + b')
  Dual a a' - Dual b b' = Dual (a - b) (a' - b')
  Dual a a' * Dual b b' = Dual (a * b) (a' * b + a * b')
  negate (Dual a a')    = Dual (negate a) (negate a')
  abs (Dual a a')       = Dual (abs a) (a' * signum a)
  signum (Dual a _)     = Dual (signum a) 0
  fromInteger n         = Dual (fromInteger n) 0

instance Fractional Dual where
  recip (Dual a a') = Dual (recip a) (negate a' / (a * a))
  fromRational r    = Dual (fromRational r) 0

instance Floating Dual where
  pi                = Dual pi 0
  exp (Dual a a')   = Dual (exp a) (a' * exp a)
  log (Dual a a')   = Dual (log a) (a' / a)
  sqrt (Dual a a')  = Dual (sqrt a) (a' / (2 * sqrt a))
  sin (Dual a a')   = Dual (sin a) (a' * cos a)
  cos (Dual a a')   = Dual (cos a) (negate a' * sin a)
  tanh (Dual a a')  = Dual (tanh a) (a' * (1 - tanh a ^ (2 :: Int)))
  asin (Dual a a')  = Dual (asin a) (a' / sqrt (1 - a * a))
  acos (Dual a a')  = Dual (acos a) (negate a' / sqrt (1 - a * a))
  atan (Dual a a')  = Dual (atan a) (a' / (1 + a * a))
  sinh (Dual a a')  = Dual (sinh a) (a' * cosh a)
  cosh (Dual a a')  = Dual (cosh a) (a' * sinh a)
  asinh (Dual a a') = Dual (asinh a) (a' / sqrt (a * a + 1))
  acosh (Dual a a') = Dual (acosh a) (a' / sqrt (a * a - 1))
  atanh (Dual a a') = Dual (atanh a) (a' / (1 - a * a))

-- 📏 The derivative of f at x: start with dx/dx = 1.
-- f の x での微分係数：dx/dx = 1 から始める
derivative :: (Dual -> Dual) -> Double -> Double
derivative f x = let Dual _ d = f (Dual x 1) in d
-- ANCHOR_END: dual
-- ANCHOR_END: all
