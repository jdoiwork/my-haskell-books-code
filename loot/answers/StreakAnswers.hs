-- 「テスト」の章のトレーニングの解答（ステップ3の素朴な版）
module StreakAnswers
  ( bestStreakNaive
  ) where

-- ANCHOR: naive
-- 🐢 The naive way: try every window of k turns, and take the best.
-- 素朴なやり方：k ターンの窓を全部試して、いちばん大きい合計を取る
bestStreakNaive :: Int -> [Int] -> Int
bestStreakNaive k damages
  | k <= 0 || length damages < k = 0
  | otherwise =
      let starts = [0 .. length damages - k]
          totals = [ sum (take k (drop i damages)) | i <- starts ]
      in  maximum totals
-- ANCHOR_END: naive
