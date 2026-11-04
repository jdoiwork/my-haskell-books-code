-- ANCHOR: module
module SwarmAnswers where

import Swarm
-- ANCHOR_END: module

-- ANCHOR: defeatedCount
-- 💀 How many goblins are defeated (HP 0) after one attack?
-- 1回攻撃したあと、倒れた（HP が 0 の）ゴブリンは何体？
defeatedCount :: Int -> [Int] -> Int
defeatedCount atk hps =
  let afterAttack = attackAll atk hps
      defeated    = filter isDefeated afterAttack
  in  length defeated
  where
    isDefeated hp = hp == 0
-- ANCHOR_END: defeatedCount

-- ANCHOR: strongestFold
-- 👑 The highest HP in the swarm, written with foldr. (0 for an empty swarm)
-- 群れの中でいちばん高い HP を、foldr で書いたもの（群れが空なら 0）
strongest' :: [Int] -> Int
strongest' hps = foldr max 0 hps
-- ANCHOR_END: strongestFold

-- ANCHOR: defeatedCountComposed
-- 💀 defeatedCount, built with function composition.
-- 関数合成で組み立てた defeatedCount
defeatedCount' :: Int -> [Int] -> Int
defeatedCount' atk = length . filter (== 0) . attackAll atk
-- ANCHOR_END: defeatedCountComposed
