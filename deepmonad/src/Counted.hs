-- ANCHOR: all
-- | A Functor instance that type-checks, but breaks the laws.
-- 型は合っているのに、法則を破る Functor のインスタンス
module Counted (Counted (..), potion) where

-- ANCHOR: counted
-- 🔢 A value, and how many times fmap has touched it.
-- 値と、fmap で何回さわったか
data Counted a = Counted Int a
  deriving (Show, Eq)

-- ❌ Counts every fmap. Looks handy, but breaks both laws.
-- fmap のたびに数える。便利そうだが、2つの法則を両方とも破る
instance Functor Counted where
  fmap f (Counted n x) = Counted (n + 1) (f x)

-- 🧪 A potion worth 5, not touched yet.
-- まだ一度もさわっていない、価値 5 のポーション
potion :: Counted Int
potion = Counted 0 5
-- ANCHOR_END: counted
-- ANCHOR_END: all
