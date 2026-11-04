-- ANCHOR: all
-- | How things grow: doubling slimes, halving guesses, and interest.
-- 増え方：2倍に増えるスライム、半分にする数当て、利息
module Math.Growth
  ( slimes
  , halvings
  , guesses
  , mergeSortCount
  , insertionSortCount
  , compound
  ) where

-- ANCHOR: slimes
-- 🟢 A slime splits in two every turn: 1, 2, 4, 8, ...
-- スライムは毎ターン2つに分かれる
slimes :: [Integer]
slimes = iterate (* 2) 1
-- ANCHOR_END: slimes

-- ANCHOR: halvings
-- ✂️ How many times can n be halved before it reaches 1?
--    halvings 1000 == 9 (1000, 500, 250, 125, 62, 31, 15, 7, 3, 1).
--    This is log2 n, rounded down.
-- n を何回半分にすると 1 になる？ これが log2 n（切り捨て）
halvings :: Int -> Int
halvings n
  | n <= 1    = 0
  | otherwise = 1 + halvings (n `div` 2)
-- ANCHOR_END: halvings

-- ANCHOR: guesses
-- 🎯 The guessing game: the number is in [lo, hi]; always guess the
--    middle. The list of guesses until we hit the target.
-- 数当てゲーム：答えは lo〜hi のどこか。いつも真ん中を言う
guesses :: Int -> Int -> Int -> [Int]
guesses lo hi target
  | lo > hi        = []
  | mid == target  = [mid]
  | mid < target   = mid : guesses (mid + 1) hi target
  | otherwise      = mid : guesses lo (mid - 1) target
  where
    mid = (lo + hi) `div` 2
-- ANCHOR_END: guesses

-- ANCHOR: sorts
-- 🔀 Merge sort, counting the comparisons: about n * log2 n.
-- マージソート。比べた回数を数える：だいたい n log2 n
mergeSortCount :: [Int] -> ([Int], Int)
mergeSortCount []  = ([], 0)
mergeSortCount [x] = ([x], 0)
mergeSortCount xs  =
  let (front, back)   = splitAt (length xs `div` 2) xs
      (sortedA, countA) = mergeSortCount front
      (sortedB, countB) = mergeSortCount back
      (merged, countM)  = merge sortedA sortedB
  in  (merged, countA + countB + countM)

-- 🤝 Merge two sorted lists, counting the comparisons.
-- 並んだ2つのリストをまとめる。比べた回数も数える
merge :: [Int] -> [Int] -> ([Int], Int)
merge [] ys = (ys, 0)
merge xs [] = (xs, 0)
merge (x : xs) (y : ys)
  | x <= y    = let (rest, c) = merge xs (y : ys) in (x : rest, c + 1)
  | otherwise = let (rest, c) = merge (x : xs) ys in (y : rest, c + 1)

-- 🐢 Insertion sort, counting the comparisons: about n^2 / 2 at worst.
-- 挿入ソート。比べた回数を数える：最悪で、だいたい n^2 / 2
insertionSortCount :: [Int] -> ([Int], Int)
insertionSortCount = foldr insert ([], 0)
  where
    insert x (sorted, count) =
      let (smaller, rest) = span (< x) sorted
          compared        = length smaller + (if null rest then 0 else 1)
      in  (smaller ++ x : rest, count + compared)
-- ANCHOR_END: sorts

-- ANCHOR: compound
-- 🏦 Money in the guild bank: principal * (1 + rate) ^ years.
-- ギルド銀行に預けたお金：元金 ×（1 + 利率）^ 年数
compound :: Double -> Double -> Int -> Double
compound principal rate years = principal * (1 + rate) ^ years
-- ANCHOR_END: compound
-- ANCHOR_END: all
