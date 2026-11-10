-- ANCHOR: all
-- ANCHOR: module
-- 🌳 A binary search tree of our own, to see how Map works inside.
-- 自作の二分探索木。Map の中身の仕組みを見るために
module Tree
  ( Tree (..)
  , empty
  , insert
  , member
  , fromList
  , toList
  , depth
  ) where
-- ANCHOR_END: module

-- ANCHOR: type
-- 🌳 A tree is empty (a leaf), or a node with a left tree, a value,
--    and a right tree. A tree has trees inside it.
-- 木は、空っぽ（葉）か、左の木・値・右の木を持つ節。
-- 木の中に、木が入っている
data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Show)
-- ANCHOR_END: type

-- ANCHOR: insert
-- 🌱 An empty tree.
-- 空っぽの木
empty :: Tree a
empty = Leaf

-- ➕ Smaller values go left, larger ones go right.
-- 小さい値は左へ、大きい値は右へ
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x node@(Node left v right)
  | x < v     = Node (insert x left) v right
  | x > v     = Node left v (insert x right)
  | otherwise = node

-- 🔍 Is it in the tree? Each step throws away half (if balanced).
-- 木の中にある？ 1歩ごとに、半分を捨てられる（つり合っていれば）
member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member x (Node left v right)
  | x < v     = member x left
  | x > v     = member x right
  | otherwise = True
-- ANCHOR_END: insert

-- ANCHOR: walk
-- 🌳 Build a tree from a list, one value at a time.
-- リストから、1つずつ足して木を作る
fromList :: Ord a => [a] -> Tree a
fromList = foldr insert empty . reverse

-- 📋 Left, then the node, then right: the values in order.
-- 左、節、右の順にたどると、小さい順に並ぶ
toList :: Tree a -> [a]
toList Leaf                = []
toList (Node left v right) = toList left ++ [v] ++ toList right

-- 📏 How many steps from the root to the deepest leaf.
-- 根から、いちばん深い葉までの段数
depth :: Tree a -> Int
depth Leaf                = 0
depth (Node left _ right) = 1 + max (depth left) (depth right)
-- ANCHOR_END: walk
-- ANCHOR_END: all
