-- ANCHOR: all
-- | A Monad instance that type-checks, but breaks the laws.
-- 型は合っているのに、法則を破る Monad のインスタンス
module Steps
  ( Steps (..)
  , walk
  , withPure
  , withoutPure
  ) where

import Control.Monad (ap)

-- ANCHOR: steps
-- 👣 A value, and how many steps it took.
-- 値と、何歩かかったか
data Steps a = Steps Int a
  deriving (Show, Eq)

instance Functor Steps where
  fmap f (Steps n x) = Steps n (f x)

instance Applicative Steps where
  pure x = Steps 0 x
  (<*>)  = ap

-- ❌ Every >>= counts one more step, on top of the steps of both sides.
-- >>= のたびに、両側の歩数に、さらに1歩を足す
instance Monad Steps where
  Steps n x >>= next =
    let Steps m y = next x
    in  Steps (n + m + 1) y
-- ANCHOR_END: steps

-- ANCHOR: refactor
-- 🚶 Walk some distance: one step per 10 meters.
-- ある距離を歩く：10 メートルで1歩
walk :: Int -> Steps Int
walk meters = Steps (meters `div` 10) meters

-- Before: name a plain value with <- and pure.
-- 書き換え前：ふつうの値に、<- と pure で名前を付ける
withPure :: Steps Int
withPure = do
  start <- pure 100
  walk start

-- After: the same thing with let. Should be the same...
-- 書き換え後：同じことを let で。同じはず……
withoutPure :: Steps Int
withoutPure = do
  let start = 100
  walk start
-- ANCHOR_END: refactor
-- ANCHOR_END: all
