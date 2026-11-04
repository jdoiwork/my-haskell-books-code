-- ANCHOR: all
module Party where

import Data.List (partition, sortOn)
import Data.Ord (Down (..))

-- ANCHOR: party
-- Adventurers and their levels
-- 冒険者と、そのレベル
party :: [(String, Int)]
party = [("Lambda", 12), ("Python", 7), ("TS", 15), ("Rust", 9)]
-- ANCHOR_END: party

-- ANCHOR: splitByLevel
-- Veterans (level 10 or more) and the others
-- ベテラン（レベル10以上）と、それ以外に分ける
splitByLevel :: [(String, Int)] -> ([(String, Int)], [(String, Int)])
splitByLevel = partition (\(_, lv) -> lv >= 10)
-- ANCHOR_END: splitByLevel

-- ANCHOR: strongestFirst
-- The strongest first
-- 強い順に並べる
strongestFirst :: [(String, Int)] -> [(String, Int)]
strongestFirst = sortOn (Down . snd)
-- ANCHOR_END: strongestFirst
-- ANCHOR_END: all
