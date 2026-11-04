-- ANCHOR: all
-- | Divide and conquer: split, solve the halves, and combine.
-- 分割統治：分けて、半分ずつ解いて、まとめる
module Algo.Divide
  ( halve
  , merge
  , msort
  , qsort
  , inversionsSpec
  , inversions
  , Run (..)
  , room
  , bestRunDC
  ) where

-- ANCHOR: msort
-- ✂️ Split a list into two halves.
-- リストを半分に分ける
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- 🤝 Merge two sorted lists into one sorted list.
-- 並んだリスト2つを、1つの並んだリストにまとめる
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y    = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- 🔀 Merge sort: split, sort each half, merge. O(n log n).
-- マージソート：分けて、半分ずつ並べて、まとめる。O(n log n)
msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  =
  let (left, right) = halve xs
  in  merge (msort left) (msort right)
-- ANCHOR_END: msort

-- ANCHOR: qsort
-- 🎯 Quicksort, the famous short one: smaller ones, the pivot, the rest.
-- 有名な短いクイックソート：小さいもの、軸、それ以外
qsort :: Ord a => [a] -> [a]
qsort []          = []
qsort (p : rest) =
  qsort [ x | x <- rest, x < p ] ++ [p] ++ qsort [ x | x <- rest, x >= p ]
-- ANCHOR_END: qsort

-- ANCHOR: inversions
-- 📜 The spec: count the pairs that are in the wrong order. O(n^2).
-- 仕様：順番が逆になっている組を数える。O(n^2)
inversionsSpec :: Ord a => [a] -> Int
inversionsSpec xs =
  length [ () | (i, a) <- zip [0 :: Int ..] xs
              , (j, b) <- zip [0 ..] xs, i < j, a > b ]

-- 🔢 Merge sort that also counts. When an element of the right half
--    goes first, it jumps over every element left in the left half.
-- 数えながらのマージソート。右の半分の要素が先に出るとき、
-- 左の半分に残っている要素を、全部飛び越している
inversions :: Ord a => [a] -> Int
inversions = fst . sortCount
  where
    sortCount []  = (0, [])
    sortCount [x] = (0, [x])
    sortCount xs  =
      let (left, right) = halve xs
          (a, ls) = sortCount left
          (b, rs) = sortCount right
          (c, ms) = mergeCount (length ls) ls rs
      in  (a + b + c, ms)
    mergeCount _ [] ys = (0, ys)
    mergeCount _ xs [] = (0, xs)
    mergeCount n (x : xs) (y : ys)
      | x <= y    = let (c, ms) = mergeCount (n - 1) xs (y : ys)
                    in  (c, x : ms)
      | otherwise = let (c, ms) = mergeCount n (x : xs) ys
                    in  (c + n, y : ms)
-- ANCHOR_END: inversions

-- ANCHOR: run
-- 🏰 What we need to know about a stretch of rooms, to combine it with
--    its neighbours: its total, the best start, the best end, the best run.
-- となりとまとめるために、部屋の並びについて知っておきたいこと：
-- 合計、左端から始まるベスト、右端で終わるベスト、中のベスト
data Run = Run { total :: Int, front :: Int, back :: Int, best :: Int }
  deriving (Show, Eq)

-- 🧩 Two neighbouring stretches make one. The best run is in the left,
--    in the right, or crosses the middle.
-- となり合う2つの並びを1つに。ベストは、左の中か、右の中か、真ん中をまたぐか
instance Semigroup Run where
  l <> r = Run
    { total = total l + total r
    , front = max (front l) (total l + front r)
    , back  = max (back r) (back l + total r)
    , best  = maximum [best l, best r, back l + front r]
    }

instance Monoid Run where
  mempty = Run 0 0 0 0

-- 🚪 One room on its own.
-- 1部屋だけ
room :: Int -> Run
room x = let v = max 0 x in Run x v v v

-- ⚔️ Divide and conquer, written as foldMap: the Monoid does the work.
-- foldMap で分割統治：Monoid が仕事をしてくれる
bestRunDC :: [Int] -> Int
bestRunDC = best . foldMap room
-- ANCHOR_END: run
-- ANCHOR_END: all
