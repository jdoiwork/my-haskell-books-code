-- ANCHOR: all
-- | One step further with a GADT: a kind that knows the Haskell type of
--   its values, so taking a value out gives the right type.
-- GADT で一歩先へ：値の Haskell の型を知っている種類。取り出すと、
-- 型がそろっている
-- ANCHOR: header
module Tsundoku.Field
  ( Kind (..)
  , fromValue
  , toValue
  , compareIn
  , SomeKind (..)
  , kindOf
  , Field (..)
  , get
  ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (Day)

import Tsundoku.Card (Card (..))
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: kind
-- 🧬 A kind of attribute, with the type of its values in its own type.
--    NumberK is a Kind Int: its values are Ints.
-- 属性の種類。値の型を、自分の型に持っている。
-- NumberK は Kind Int。その値は Int
data Kind a where
  TextK   :: Kind Text
  NumberK :: Kind Int
  DateK   :: Kind Day
  YesNoK  :: Kind Bool
  ChoiceK :: Kind Text

-- 📤 Take a value out, as the type the kind promises. In each line,
--    GHC knows what `a` is: Text, Int, Day, ...
-- 種類が約束する型で、値を取り出す。どの行でも、GHC は a が何か
-- （Text、Int、Day……）を知っている
fromValue :: Kind a -> AttrValue -> Maybe a
fromValue TextK   (TextV   t) = Just t
fromValue NumberK (NumberV n) = Just n
fromValue DateK   (DateV   d) = Just d
fromValue YesNoK  (YesNoV  b) = Just b
fromValue ChoiceK (ChoiceV c) = Just c
fromValue _       _           = Nothing

-- 📥 And put one in.
-- そして、入れる
toValue :: Kind a -> a -> AttrValue
toValue TextK   = TextV
toValue NumberK = NumberV
toValue DateK   = DateV
toValue YesNoK  = YesNoV
toValue ChoiceK = ChoiceV

-- ⚖️ Compare two values of a kind. Each line uses the `compare` of
--    its own type.
-- 同じ種類の値を2つ比べる。どの行も、その型の compare を使う
compareIn :: Kind a -> a -> a -> Ordering
compareIn TextK   = compare
compareIn NumberK = compare
compareIn DateK   = compare
compareIn YesNoK  = compare
compareIn ChoiceK = compare
-- ANCHOR_END: kind

-- ANCHOR: some
-- 🎁 A kind whose type we only learn at run time (from a definition
--    kept as data). Opening the box tells us the type again.
-- 型が、動かしてみるまでわからない種類（データとして持っている定義から
-- 作る）。箱を開けると、型がまたわかる
data SomeKind where
  SomeKind :: Kind a -> SomeKind

-- 🔁 From a definition kept as data, to a kind that knows its type.
-- データとして持っている定義から、型を知っている種類へ
kindOf :: AttrKind -> SomeKind
kindOf kind = case kind of
  TextKind     -> SomeKind TextK
  NumberKind   -> SomeKind NumberK
  DateKind     -> SomeKind DateK
  YesNoKind    -> SomeKind YesNoK
  ChoiceKind _ -> SomeKind ChoiceK
-- ANCHOR_END: some

-- ANCHOR: field
-- 🔑 An attribute the program knows: its name and its kind.
-- プログラムが知っている属性：名前と種類
data Field a = Field Text (Kind a)

-- 🔍 The value of a field on a card, as the field's type.
-- カードの、その属性の値。属性の型で
get :: Field a -> Card -> Maybe a
get (Field name kind) card = do
  value <- Map.lookup name (attrs card)
  fromValue kind value
-- ANCHOR_END: field
-- ANCHOR_END: all
