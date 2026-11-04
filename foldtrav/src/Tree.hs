-- ANCHOR: all
-- | The binary search tree from "Choosing data structures", again,
--   now with Functor and Foldable instances written by hand.
-- 「データ構造の選び方」の二分探索木を、もう一度。
-- 今度は Functor と Foldable のインスタンスを手で書く
module Tree
  ( Tree (..)
  , insert
  , fromList
  , bag
  ) where

-- ANCHOR: type
-- 🌳 Empty, or a node with a left tree, a value and a right tree.
-- 空っぽか、左の木・値・右の木を持つ節
data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

-- ➕ Smaller values go left, larger ones go right.
-- 小さい値は左へ、大きい値は右へ
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x node@(Node left v right)
  | x < v     = Node (insert x left) v right
  | x > v     = Node left v (insert x right)
  | otherwise = node

fromList :: Ord a => [a] -> Tree a
fromList = foldl (flip insert) Leaf

-- 🎒 The adventurer's bag: item names, kept in a tree.
-- 冒険者の袋：アイテムの名前を、木にしまっておく
bag :: Tree String
bag = fromList ["potion", "herb", "sword", "elixir", "shield"]
-- ANCHOR_END: type

-- ANCHOR: functor
-- 🧺 Functor: change every value, keep the shape of the tree.
-- Functor：値を全部変える。木の形はそのまま
instance Functor Tree where
  fmap _ Leaf                = Leaf
  fmap f (Node left v right) = Node (fmap f left) (f v) (fmap f right)
-- ANCHOR_END: functor

-- ANCHOR: foldable
-- 🧮 Foldable: say how to turn every value into a Monoid and join them,
--    left tree, then the node, then the right tree.
-- Foldable：値を1つずつ Monoid にして、つなぐ方法を言う。
-- 左の木、節、右の木の順に
instance Foldable Tree where
  foldMap _ Leaf                = mempty
  foldMap f (Node left v right) = foldMap f left <> f v <> foldMap f right
-- ANCHOR_END: foldable

-- ANCHOR: traversable
-- 🚶 Traversable: visit every value with an action, and rebuild the
--    same tree from the results, in the same order as foldMap.
-- Traversable：値を1つずつ手順に通し、結果から同じ形の木を組み立て直す。
-- 順番は foldMap と同じ
instance Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Node left v right) =
    Node <$> traverse f left <*> f v <*> traverse f right
-- ANCHOR_END: traversable
-- ANCHOR_END: all
