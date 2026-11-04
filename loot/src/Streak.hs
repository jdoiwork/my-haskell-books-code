-- ANCHOR: all
-- ANCHOR: module
-- ⚡ The best streak: the largest total damage in k turns in a row.
-- 最高の連続攻撃：連続する k ターンのダメージの合計の、いちばん大きいもの
module Streak
  ( bestStreak
  ) where
-- ANCHOR_END: module

-- ANCHOR: fast
-- | ⚡ The fast way: slide a window of k turns.
--    Moving one turn to the right, add the new turn and drop the old one.
--    0 when there are fewer than k turns (or k is not positive).
-- 速いやり方：k ターンの窓を1つずつずらす
-- 右に1つずらすたびに、新しいターンを足して、古いターンを引く
-- ターンが k より少ない（または k が正でない）ときは 0
--
-- >>> bestStreak 3 [5, 1, 8, 2, 9, 1]
-- 19
bestStreak :: Int -> [Int] -> Int
bestStreak k damages
  | k <= 0 || length damages < k = 0
  | otherwise =
      let first  = sum (take k damages)
          moves  = zipWith (-) (drop k damages) damages
          totals = scanl (+) first moves
      in  maximum totals
-- ANCHOR_END: fast
-- ANCHOR_END: all
