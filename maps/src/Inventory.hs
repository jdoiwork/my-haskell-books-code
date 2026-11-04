-- ANCHOR: module
module Inventory where

-- Map: a dictionary type. "qualified as Map" means we write Map.lookup etc.
-- Map：辞書の型。「qualified as Map」は、
-- Map.lookup のように接頭辞を付けて使うという意味
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
-- ANCHOR_END: module

-- ANCHOR: bag
-- 🎒 What is in the adventurer's bag: item name -> how many.
-- 冒険者の袋の中身：アイテム名 → 個数
bag :: Map String Int
bag = Map.fromList [("potion", 3), ("herb", 1), ("elixir", 0)]
-- ANCHOR_END: bag

-- ANCHOR: howMany
-- 🔍 How many of this item do we have? (0 if we have never had it)
-- このアイテムをいくつ持っている？（一度も持ったことがなければ 0）
howMany :: String -> Map String Int -> Int
howMany item inventory = Map.findWithDefault 0 item inventory
-- ANCHOR_END: howMany

-- ANCHOR: addItem
-- ➕ Put n of this item into the bag.
--    If we already have some, add to the count.
-- アイテムを n 個、袋に入れる。すでに持っていれば個数を足す
addItem :: String -> Int -> Map String Int -> Map String Int
addItem item n inventory = Map.insertWith (+) item n inventory
-- ANCHOR_END: addItem

-- ANCHOR: useItem
-- 🧪 Use one of this item, if we have any.
-- アイテムを1つ使う（持っていれば）
useItem :: String -> Map String Int -> Map String Int
useItem item inventory
  | howMany item inventory > 0 = Map.adjust (subtract 1) item inventory
  | otherwise                  = inventory
-- ANCHOR_END: useItem

-- ANCHOR: updateAlter
-- 🧪 For Map.update: use one. When it runs out, remove the key.
-- Map.update 用：1つ使う。なくなったら、キーごと消す
useOne :: Int -> Maybe Int
useOne n
  | n > 1     = Just (n - 1)
  | otherwise = Nothing

-- 🎒 For Map.alter: add one, whether or not we already have it.
-- Map.alter 用：持っていてもいなくても、1つ足す
addOne :: Maybe Int -> Maybe Int
addOne Nothing  = Just 1
addOne (Just n) = Just (n + 1)
-- ANCHOR_END: updateAlter

-- ANCHOR: inStock
-- 🧹 Keep only the items we still have (count > 0).
-- まだ持っているアイテム（個数が 0 より多いもの）だけを残す
inStock :: Map String Int -> Map String Int
inStock inventory = Map.filter (> 0) inventory

-- 📦 The total number of items in the bag.
-- 袋の中のアイテムの合計個数
totalItems :: Map String Int -> Int
totalItems inventory = sum (Map.elems inventory)
-- ANCHOR_END: inStock

-- ANCHOR: countDrops
-- 💰 Count the items dropped by the enemies, from a list of item names.
-- 敵が落としたアイテムの名前のリストから、アイテムごとの個数を数える
countDrops :: [String] -> Map String Int
countDrops drops = Map.fromListWith (+) [ (item, 1) | item <- drops ]
-- ANCHOR_END: countDrops

-- ANCHOR: battleLog
-- 📝 Which enemy was defeated on which turn: (turn, name).
-- どのターンにどの敵を倒したか：（ターン, 名前）
battleLog :: [(Int, String)]
battleLog =
  [ (1, "Goblin-A")
  , (2, "Goblin-C")
  , (1, "Goblin-B")
  , (3, "Goblin-D")
  , (3, "Goblin-E")
  ]
-- ANCHOR_END: battleLog

-- ANCHOR: defeatedByTurn
-- 🗂️ Group the names by turn, keeping the order of the log.
-- 名前をターンごとにまとめる（ログの順番は保つ）
defeatedByTurn :: [(Int, String)] -> Map Int [String]
defeatedByTurn entries =
  let pairs = [ (turn, [name]) | (turn, name) <- entries ]
  in  Map.fromListWith (\new old -> old ++ new) pairs
-- ANCHOR_END: defeatedByTurn

-- ANCHOR: mergeBags
-- 🤝 Put two bags together. Counts of the same item are added up.
-- 2つの袋をひとつにまとめる。同じアイテムの個数は足し合わせる
mergeBags :: Map String Int -> Map String Int -> Map String Int
mergeBags a b = Map.unionWith (+) a b
-- ANCHOR_END: mergeBags
