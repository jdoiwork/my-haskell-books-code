{-# LANGUAGE TypeFamilies #-}
-- ANCHOR: all
-- | A list that knows its length in its type.
-- 長さを型に持つリスト
module Vec
  ( Nat (..)
  , Vec (..)
  , vhead
  , vtail
  , vzip
  , vtoList
  , Plus
  , vappend
  , party
  , Three
  ) where

-- ANCHOR: vec
-- 🔢 Numbers for the type level: zero, or one more than a number.
-- 型の世界の数：ゼロか、ある数の次
data Nat = Z | S Nat

-- 📏 A Vec n a holds exactly n values of type a.
-- Vec n a には、型 a の値が、ちょうど n 個入っている
data Vec (n :: Nat) a where
  VNil :: Vec Z a
  (:>) :: a -> Vec n a -> Vec (S n) a
infixr 5 :>

deriving instance Show a => Show (Vec n a)
deriving instance Functor (Vec n)
deriving instance Foldable (Vec n)

type Three = S (S (S Z))

-- 👥 A party of exactly three.
-- ちょうど3人のパーティー
party :: Vec Three String
party = "Lambda" :> "Python" :> "Rust" :> VNil
-- ANCHOR_END: vec

-- ANCHOR: safe
-- 👑 The first one. Only for a Vec with at least one: never fails.
-- 先頭。1つ以上入った Vec にしか使えない：失敗しない
vhead :: Vec (S n) a -> a
vhead (x :> _) = x

vtail :: Vec (S n) a -> Vec n a
vtail (_ :> xs) = xs

-- 🤝 Pair up two Vecs of the same length; the result has that length too.
-- 同じ長さの Vec を組にする。結果も、同じ長さ
vzip :: Vec n a -> Vec n b -> Vec n (a, b)
vzip VNil      VNil      = VNil
vzip (x :> xs) (y :> ys) = (x, y) :> vzip xs ys

vtoList :: Vec n a -> [a]
vtoList = foldr (:) []
-- ANCHOR_END: safe

-- ANCHOR: plus
-- ➕ Addition on the type level, as a type family.
-- 型の世界の足し算を、型族で
type family Plus (n :: Nat) (m :: Nat) :: Nat where
  Plus Z     m = m
  Plus (S n) m = S (Plus n m)

-- 🔗 Joining: the length of the result is the sum of the lengths.
-- つなぐ：結果の長さは、長さの和
vappend :: Vec n a -> Vec m a -> Vec (Plus n m) a
vappend VNil      ys = ys
vappend (x :> xs) ys = x :> vappend xs ys
-- ANCHOR_END: plus
-- ANCHOR_END: all
