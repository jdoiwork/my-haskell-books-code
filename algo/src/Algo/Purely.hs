-- ANCHOR: all
-- | Purely functional data structures: a queue made of two lists,
--   and a leftist heap. Old versions stay usable.
-- 純粋なデータ構造：2つのリストでできたキューと、左偏ヒープ。
-- 古い版も、そのまま使える
module Algo.Purely
  ( -- * A queue made of one list (slow)
    SlowQueue (..)
  , slowEmpty
  , slowPush
  , slowPop
    -- * A queue made of two lists
  , Queue
  , emptyQueue
  , push
  , pop
  , queueFromList
  , drain
  , queueInvariant
    -- * A leftist heap
  , Heap (..)
  , emptyHeap
  , rank
  , merge
  , insertHeap
  , findMin
  , deleteMin
  , heapFromList
  , heapSort
  , leftist
  ) where

-- ANCHOR: imports
import Data.List (unfoldr)
-- ANCHOR_END: imports

-- ANCHOR: slow
-- 🐢 A queue as one list: the front of the line is the head.
--    Pushing walks to the end of the list, so it is O(n).
-- 1つのリストのキュー：先頭が列の先頭。
-- 後ろに並ぶたびに、リストの終わりまで歩くので O(n)
newtype SlowQueue a = SlowQueue [a]
  deriving Show

slowEmpty :: SlowQueue a
slowEmpty = SlowQueue []

slowPush :: a -> SlowQueue a -> SlowQueue a
slowPush x (SlowQueue xs) = SlowQueue (xs ++ [x])

slowPop :: SlowQueue a -> Maybe (a, SlowQueue a)
slowPop (SlowQueue []      ) = Nothing
slowPop (SlowQueue (x : xs)) = Just (x, SlowQueue xs)
-- ANCHOR_END: slow

-- ANCHOR: queue
-- 🎟️ A queue as two lists:
--    front: the head of the line, in order (we pop from here)
--    back:  the tail of the line, newest first (we push here)
-- 2つのリストのキュー：
--   front は列の前半（順番どおり。ここから取り出す）
--   back は列の後半（新しい順。ここに並ぶ）
data Queue a = Queue [a] [a]
  deriving Show

emptyQueue :: Queue a
emptyQueue = Queue [] []

-- ➕ Join the line: put it on the front of back. O(1).
-- 列に並ぶ：back の先頭に置くだけ。O(1)
push :: a -> Queue a -> Queue a
push x (Queue front back) = check (Queue front (x : back))

-- ➖ Leave the line from the front.
-- 列の先頭から抜ける
pop :: Queue a -> Maybe (a, Queue a)
pop (Queue []          _   ) = Nothing
pop (Queue (x : front) back) = Just (x, check (Queue front back))

-- 🔄 Keep the rule "front is empty only when the whole queue is":
--    when front runs out, turn back around once.
-- 「front が空なのは、キューが空のときだけ」を守る：
-- front がなくなったら、back を1回だけひっくり返す
check :: Queue a -> Queue a
check (Queue [] back) = Queue (reverse back) []
check q               = q
-- ANCHOR_END: queue

-- ANCHOR: queueHelpers
-- 📥 Push everything in order.
-- 順番に全部並べる
queueFromList :: [a] -> Queue a
queueFromList = foldl' (flip push) emptyQueue

-- 📤 Pop until empty.
-- 空になるまで取り出す
drain :: Queue a -> [a]
drain = unfoldr pop
-- ANCHOR_END: queueHelpers

-- ✅ The rule that check keeps.
-- check が守っている決まり
queueInvariant :: Queue a -> Bool
queueInvariant (Queue [] back) = null back
queueInvariant _               = True

-- ANCHOR: heap
-- 🏔️ A leftist heap: the smallest value sits at the top, and the left
--    child's rank (the length of the rightmost path) is never smaller
--    than the right child's.
-- 左偏ヒープ：いちばん小さい値がてっぺんにあり、左の子のランク
-- （右端をたどる道の長さ）は、右の子のランクより小さくならない
data Heap a = Empty | Node Int a (Heap a) (Heap a)
  deriving Show

emptyHeap :: Heap a
emptyHeap = Empty

rank :: Heap a -> Int
rank Empty          = 0
rank (Node r _ _ _) = r

-- 🤝 Merge two heaps: keep the smaller top, and merge down the
--    right side. The right path is short, so this is O(log n).
-- 2つのヒープを合わせる：小さいほうのてっぺんを残し、右側へ
-- 合わせていく。右の道は短いので O(log n)
merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty h     = h
merge h     Empty = h
merge h1@(Node _ x left1 right1) h2@(Node _ y _ _)
  | x <= y    = makeNode x left1 (merge right1 h2)
  | otherwise = merge h2 h1

-- 🧭 Put the child with the larger rank on the left.
-- ランクの大きい子を左に置く
makeNode :: a -> Heap a -> Heap a -> Heap a
makeNode x a b
  | rank a >= rank b = Node (rank b + 1) x a b
  | otherwise        = Node (rank a + 1) x b a
-- ANCHOR_END: heap

-- ANCHOR: heapOps
-- ➕ Insert: merge with a heap of one.
-- 入れる：1つだけのヒープと合わせる
insertHeap :: Ord a => a -> Heap a -> Heap a
insertHeap x = merge (Node 1 x Empty Empty)

-- 👀 The smallest value is at the top. O(1).
-- いちばん小さい値は、てっぺん。O(1)
findMin :: Heap a -> Maybe a
findMin Empty          = Nothing
findMin (Node _ x _ _) = Just x

-- ➖ Remove the top: merge its two children.
-- てっぺんを取る：2つの子を合わせる
deleteMin :: Ord a => Heap a -> Maybe (a, Heap a)
deleteMin Empty                 = Nothing
deleteMin (Node _ x left right) = Just (x, merge left right)
-- ANCHOR_END: heapOps

-- ANCHOR: heapSort
heapFromList :: Ord a => [a] -> Heap a
heapFromList = foldr insertHeap emptyHeap

-- 🔢 Take the smallest, again and again.
-- いちばん小さいものを、くり返し取り出す
heapSort :: Ord a => [a] -> [a]
heapSort xs = unfoldr deleteMin (heapFromList xs)
-- ANCHOR_END: heapSort

-- ✅ Every node: the top is the smallest, the left rank is not
--    smaller, and the stored rank is right.
-- どの節も：てっぺんが最小、左のランクが小さくない、
-- 覚えているランクが正しい
leftist :: Ord a => Heap a -> Bool
leftist Empty = True
leftist (Node r x left right) =
  r == rank right + 1
    && rank left >= rank right
    && all (x <=) (findMin left)
    && all (x <=) (findMin right)
    && leftist left
    && leftist right
-- ANCHOR_END: all
