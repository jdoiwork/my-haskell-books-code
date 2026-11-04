-- ANCHOR: all
-- | The best run of rooms in a dungeon: from an obvious specification
--   to a fast program, one equal step at a time.
-- ダンジョンでいちばんよい連続した部屋：わかりやすい仕様から、
-- 等しさを保つ変形を1歩ずつ重ねて、速いプログラムへ
module Algo.Thinking
  ( segments
  , bestRun0
  , bestRun1
  , bestRun2
  , bestRun3
  , bestRun
  , dungeon
  ) where

import Data.List (inits, tails)

-- ANCHOR: dungeon
-- 🏰 Treasure in each room: plus for gold, minus for traps.
-- 部屋ごとの宝：ゴールドはプラス、罠はマイナス
dungeon :: [Int]
dungeon = [3, -5, 4, -1, 2, 6, -8, 5, -2]
-- ANCHOR_END: dungeon

-- ANCHOR: spec
-- 🧾 Every run of neighbouring rooms (including the empty run).
-- となり合う部屋の並び（空の並びも入れて）を、全部
segments :: [a] -> [[a]]
segments xs = [ run | rest <- tails xs, run <- inits rest ]

-- 📜 The specification: the largest total over all runs. O(n^3).
-- 仕様：すべての並びの合計のうち、いちばん大きいもの。O(n^3)
bestRun0 :: [Int] -> Int
bestRun0 xs = maximum (map sum (segments xs))
-- ANCHOR_END: spec

-- ANCHOR: step1
-- 1️⃣ Step 1: group the runs by where they start.
--    (maximum of everything = maximum of the group maximums)
-- 1歩目：並びを、始まる部屋ごとにまとめる。
-- （全体の最大 ＝ グループごとの最大の、最大）
bestRun1 :: [Int] -> Int
bestRun1 xs = maximum [ bestStart rest | rest <- tails xs ]
  where
    bestStart rest = maximum (map sum (inits rest))
-- ANCHOR_END: step1

-- ANCHOR: step2
-- 2️⃣ Step 2: "the sums of every beginning" is a running total, scanl.
--    O(n^2)
-- 2歩目：「先頭からの合計を全部」は、累計の scanl。O(n^2)
bestRun2 :: [Int] -> Int
bestRun2 xs = maximum [ bestStart rest | rest <- tails xs ]
  where
    bestStart rest = maximum (scanl (+) 0 rest)
-- ANCHOR_END: step2

-- ANCHOR: step3
-- 3️⃣ Step 3: bestStart is a foldr; and foldr over every tail is scanr.
--    O(n)
-- 3歩目：bestStart は foldr。そして、すべての後ろ半分への foldr は scanr。
-- O(n)
bestRun3 :: [Int] -> Int
bestRun3 xs = maximum (scanr step 0 xs)
  where
    step x best = max 0 (x + best)
-- ANCHOR_END: step3

-- ANCHOR: final
-- 🏆 The same thing in one pass, keeping two numbers (Kadane's algorithm).
-- 同じことを1回の走査で。2つの数を持ち回る（カデインのアルゴリズム）
bestRun :: [Int] -> Int
bestRun = fst . foldr step (0, 0)
  where
    step x (best, here) =
      let here' = max 0 (x + here)
      in  (max best here', here')
-- ANCHOR_END: final
-- ANCHOR_END: all
