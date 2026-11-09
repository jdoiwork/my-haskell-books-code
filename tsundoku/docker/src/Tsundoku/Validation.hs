-- ANCHOR: all
-- | An Applicative that keeps every problem, instead of stopping at the
--   first one like Either. (The same one we made in the chapter on
--   Functor, Applicative and Monad.)
-- Either のように最初で止まらず、まずい所を全部集める Applicative
-- （Functor・Applicative・Monad をもっと深くの章で作ったものと同じ）
module Tsundoku.Validation
  ( Validation (..)
  , problem
  , checked
  ) where

import Tsundoku.Types (Checked, Mistake)

-- 📋 All good, or a pile of problems.
-- 全部よいか、まずい所の山か
data Validation e a
  = Invalid e
  | Valid a
  deriving (Show, Eq)

instance Functor (Validation e) where
  fmap _ (Invalid e) = Invalid e
  fmap f (Valid a)   = Valid (f a)

-- 🤝 When both sides are Invalid, keep both piles (<>).
-- 両方が Invalid なら、両方の山を残す（<>）
instance Semigroup e => Applicative (Validation e) where
  pure = Valid
  Invalid e1 <*> Invalid e2 = Invalid (e1 <> e2)
  Invalid e1 <*> Valid _    = Invalid e1
  Valid f    <*> v          = fmap f v

-- 🚧 One problem, as a pile of one.
-- まずい所を1つ、1つだけの山にする
problem :: e -> Validation [e] a
problem e = Invalid [e]

-- ✅ Back to the core's answer: the value, or one mistake that holds
--    the whole pile.
-- 核の答えに戻す：値か、山の全部を持ったまちがい1つか
checked :: ([e] -> Mistake) -> Validation [e] a -> Checked a
checked _    (Valid a)   = Right a
checked what (Invalid e) = Left (what e)
-- ANCHOR_END: all
