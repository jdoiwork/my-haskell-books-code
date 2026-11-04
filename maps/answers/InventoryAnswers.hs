-- ANCHOR: module
module InventoryAnswers where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
-- ANCHOR_END: module

-- ANCHOR: dropItem
-- 🗑️ Throw away an item completely.
-- アイテムを丸ごと捨てる
dropItem :: String -> Map String Int -> Map String Int
dropItem item inventory = Map.delete item inventory
-- ANCHOR_END: dropItem

-- ANCHOR: doubleAll
-- ✨ A lucky event: every item count is doubled.
-- ラッキーイベント：すべてのアイテムの個数が2倍になる
doubleAll :: Map String Int -> Map String Int
doubleAll inventory = Map.map (* 2) inventory
-- ANCHOR_END: doubleAll

-- ANCHOR: wordCounts
-- 📖 Count how many times each word appears in a text.
-- 文章の中で、それぞれの単語が何回出てくるかを数える
wordCounts :: String -> Map String Int
wordCounts text = Map.fromListWith (+) [ (w, 1) | w <- words text ]
-- ANCHOR_END: wordCounts
