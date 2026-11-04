-- ANCHOR: all
-- | The same tree, with the instances made by deriving.
-- 同じ木を、deriving でインスタンスを作って
module Derived
  ( Tree (..)
  ) where

-- ANCHOR: derived
-- 🪄 GHC writes fmap, foldMap and traverse for us.
-- fmap も foldMap も traverse も、GHC が書いてくれる
data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq, Functor, Foldable, Traversable)
-- ANCHOR_END: derived
-- ANCHOR_END: all
