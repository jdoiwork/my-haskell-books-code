-- ANCHOR: via
{-# LANGUAGE DerivingVia #-}
-- | A column: borrow the instances of Sum Int with DerivingVia.
-- コラム：DerivingVia で、Sum Int のインスタンスを借りる
module ViaSum (Pages (..)) where

import Data.Monoid (Sum (..))

-- ➕ Pages add up just like Sum Int does.
-- Pages の足し算は、Sum Int とまったく同じ
newtype Pages = Pages Int
  deriving (Show, Eq, Ord)
  deriving (Semigroup, Monoid) via (Sum Int)
-- ANCHOR_END: via
