-- ANCHOR: module
module BattleLogAnswers where

import Data.List (group, unfoldr)

import BattleLog
-- ANCHOR_END: module

-- ANCHOR: totalDamage
-- 💥 The total damage dealt by the whole party.
-- パーティー全員で与えたダメージの合計
totalDamage :: [Int] -> [Int] -> Int
totalDamage attacks defenses = sum (zipWith damage attacks defenses)
-- ANCHOR_END: totalDamage

-- ANCHOR: countdown
-- ⏳ Count down from n to 1. (unfoldr)
-- n から 1 までカウントダウンする（unfoldr）
countdown :: Int -> [Int]
countdown n = unfoldr step n
  where
    step 0 = Nothing
    step k = Just (k, k - 1)
-- ANCHOR_END: countdown

-- ANCHOR: runLengths
-- 📦 How many times each value repeats in a row. (group)
-- 同じ値が何回続いたか（group）
runLengths :: [Int] -> [Int]
runLengths xs = map length (group xs)
-- ANCHOR_END: runLengths
