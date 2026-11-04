{-# LANGUAGE OverloadedRecordDot #-}
-- ANCHOR: all
-- | A tiny lens library, to see how lens works inside.
-- lens の中身を見るための、小さな lens ライブラリ
module MiniLens
  ( Lens
  , Traversal
  , lens
  , view
  , over
  , set
  , Point (..)
  , px
  , py
  ) where

import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))

-- ANCHOR: types
-- 🔭 A lens: given a way to "do something" with the part, do it with
--    the whole. f decides what "do something" means.
-- レンズ：部分に「何かする」方法をもらって、全体に同じことをする。
-- 「何か」が何なのかは、f が決める
type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

-- 🚶 A traversal: the same, for many parts at once (Applicative).
-- トラバーサル：同じことを、たくさんの部分に一度に（Applicative）
type Traversal s a = forall f. Applicative f => (a -> f a) -> s -> f s

-- 🔧 Build a lens from a getter and a setter.
-- 取り出す関数と、入れ替える関数から、レンズを作る
lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens getter setter f s = fmap (setter s) (f (getter s))
-- ANCHOR_END: types

-- ANCHOR: use
-- 👀 view: "do something" = remember the part (Const), ignore the rest.
-- view：「何か」＝ 部分を覚えておく（Const）。残りは気にしない
view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
view l s = getConst (l Const s)

-- ✏️ over: "do something" = change the part (Identity).
-- over：「何か」＝ 部分を変える（Identity）
over :: ((a -> Identity a) -> s -> Identity s) -> (a -> a) -> s -> s
over l f s = runIdentity (l (Identity . f) s)

set :: ((a -> Identity a) -> s -> Identity s) -> a -> s -> s
set l a = over l (const a)
-- ANCHOR_END: use

-- ANCHOR: point
-- 📍 A point, and lenses for its two parts.
-- 点と、その2つの部分のレンズ
data Point = Point { x :: Int, y :: Int }
  deriving (Show, Eq)

px :: Lens Point Int
px = lens (\p -> p.x) (\p v -> p { x = v })

py :: Lens Point Int
py = lens (\p -> p.y) (\p v -> p { y = v })
-- ANCHOR_END: point
-- ANCHOR_END: all
