-- ANCHOR: module
module Enemies where
-- ANCHOR_END: module

-- ANCHOR: import
import Data.List (sort)
-- ANCHOR_END: import

-- ANCHOR: enemies
-- 👿 The HP of each enemy goblin in the swarm.
-- 敵（ゴブリンの群れ）の、それぞれの HP
enemies :: [Int]
enemies = [30, 5, 80, 12, 45]
-- ANCHOR_END: enemies

-- ANCHOR: names
-- 🏷️ The names of the enemies, in the same order.
-- 敵の名前（HP と同じ順番）
names :: [String]
names = ["Goblin-A", "Goblin-B", "Goblin-C", "Goblin-D", "Goblin-E"]
-- ANCHOR_END: names

-- ANCHOR: roster
-- 📋 Pair each name with its HP.
-- 名前と HP を組にする
roster :: [(String, Int)]
roster = zip names enemies
-- ANCHOR_END: roster

-- ANCHOR: weakestFirst
-- 🔽 Sort the HP from the weakest to the strongest.
-- HP を弱い順（小さい順）に並べる
weakestFirst :: [Int] -> [Int]
weakestFirst hps = sort hps
-- ANCHOR_END: weakestFirst

-- ANCHOR: countGoblins
-- 📏 Count the goblins, one by one. (recursion)
-- ゴブリンを1体ずつ数える（再帰）
countGoblins :: [Int] -> Int
-- no goblins left: 0
-- もういない：0 体
countGoblins []       = 0
-- one goblin + the rest
-- 1体 + 残り
countGoblins (_ : xs) = 1 + countGoblins xs
-- ANCHOR_END: countGoblins

-- ANCHOR: totalHp
-- 🧮 Add up the HP of all the goblins. (recursion)
-- すべてのゴブリンの HP を合計する（再帰）
totalHp :: [Int] -> Int
-- no goblins: 0
-- いなければ 0
totalHp []       = 0
-- first HP + the rest
-- 先頭の HP + 残りの合計
totalHp (x : xs) = x + totalHp xs
-- ANCHOR_END: totalHp

-- ANCHOR: strongest
-- 👑 The highest HP in the swarm. (recursion; 0 for an empty swarm)
-- 群れの中でいちばん高い HP（再帰。群れが空なら 0）
strongest :: [Int] -> Int
-- no goblins: 0
-- いなければ 0
strongest []       = 0
-- the bigger one
-- 先頭と残りの最強の、大きいほう
strongest (x : xs) = max x (strongest xs)
-- ANCHOR_END: strongest
