-- ANCHOR: all
-- | 👾 How many times did each kind of monster show up in the dungeon?
-- ダンジョンで、モンスターの種類ごとに、何回出会った？
module Tally
  ( monsterKinds
  , encounters
  , tallyMap
  , tallyVector
  , tallyAccum
  ) where

-- ANCHOR: imports
-- The loop, and the ST workshop.
-- ループと、ST の作業場
import Control.Monad    (forM_)
import Control.Monad.ST (runST)

-- Map, and the two kinds of unboxed vectors.
-- Map と、2種類の Unboxed の Vector
import Data.Map.Strict             qualified as Map
import Data.Vector.Unboxed         qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU
-- ANCHOR_END: imports

-- ANCHOR: encounters
-- 👾 There are 1,000 kinds of monsters, numbered 0 to 999.
-- モンスターは 1,000 種類。0〜999 の番号で呼ぶ
monsterKinds :: Int
monsterKinds = 1000

-- 🎲 The kinds of the first n monsters we meet (a fixed random sequence).
-- 出会った n 体の種類（決まった乱数の列）
encounters :: Int -> [Int]
encounters n = take n (map kind (drop 1 (iterate step 42)))
  where
    step x = (x * 1103515245 + 12345) `mod` 2147483648
    kind x = (x `div` 65536) `mod` monsterKinds
-- ANCHOR_END: encounters

-- ANCHOR: map
-- 🗺️ Count with a Map: a new Map for every monster.
-- Map で数える。1体ごとに、新しい Map ができる
tallyMap :: [Int] -> Map.Map Int Int
tallyMap kinds = foldl' countOne Map.empty kinds
  where
    countOne counts k = Map.insertWith (+) k 1 counts
-- ANCHOR_END: map

-- ANCHOR: vector
-- 🔧 Count by rewriting 1,000 boxes in place.
-- 1,000 個の箱を、その場で書き換えて数える
tallyVector :: [Int] -> VU.Vector Int
tallyVector kinds = runST $ do
  counts <- MVU.replicate monsterKinds 0
  forM_ kinds $ \k ->
    MVU.modify counts (+ 1) k
  VU.freeze counts
-- ANCHOR_END: vector

-- ANCHOR: accum
-- 🧺 The same, with a pure function that rewrites for us.
-- 同じことを、中で書き換えてくれる純粋な関数で
tallyAccum :: [Int] -> VU.Vector Int
tallyAccum kinds =
  VU.accum (+) (VU.replicate monsterKinds 0) [(k, 1) | k <- kinds]
-- ANCHOR_END: accum
-- ANCHOR_END: all
