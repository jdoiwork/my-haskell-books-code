-- ANCHOR: all
-- | The three classes, written out again (simplified from base),
--   with the instances for Maybe.
--   The real ones live in the Prelude; these hide them, only to read.
-- 3つの型クラスを、もう一度書いてみる（base を簡単にしたもの）。
-- 本物は Prelude にある。これは読むためだけに、本物を隠して書いた
module Recap
  ( Functor (..)
  , (<$>)
  , Applicative (..)
  , Monad (..)
  ) where

import Prelude hiding (Applicative (..), Functor (..), Monad (..), (<$>))

-- ANCHOR: functor
-- 📦 Functor: change what is inside, leave the box alone.
-- Functor：箱はそのまま、中身を変える
class Functor f where
  fmap :: (a -> b) -> f a -> f b

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

infixl 4 <$>

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap g (Just x) = Just (g x)
-- ANCHOR_END: functor

-- ANCHOR: applicative
-- 🧩 Applicative: combine boxes that do not depend on each other.
-- Applicative：おたがいに頼らない箱を組み合わせる
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

infixl 4 <*>

instance Applicative Maybe where
  pure = Just
  Just g <*> Just x = Just (g x)
  _      <*> _      = Nothing
-- ANCHOR_END: applicative

-- ANCHOR: monad
-- 🔗 Monad: the next step may look at the result of the last one.
-- Monad：次の手順が、前の手順の結果を見て決まる
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b

infixl 1 >>=

instance Monad Maybe where
  Nothing >>= _    = Nothing
  Just x  >>= next = next x
-- ANCHOR_END: monad
-- ANCHOR_END: all
