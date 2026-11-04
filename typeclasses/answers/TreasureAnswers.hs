module TreasureAnswers where

import Data.List (sortOn)

import Treasure

-- ANCHOR: isRareOrBetter
-- ✨ Is this rarity Rare or rarer?
-- このレア度は Rare 以上？
isRareOrBetter :: Rarity -> Bool
isRareOrBetter r = r >= Rare
-- ANCHOR_END: isRareOrBetter

-- ANCHOR: sameRarity
-- 🤝 Do these two items have the same rarity?
-- 2つのアイテムは、同じレア度？
sameRarity :: Item -> Item -> Bool
sameRarity a b = rarity a == rarity b
-- ANCHOR_END: sameRarity

-- ANCHOR: byName
-- 🔤 Sort items by name (in alphabetical order).
-- アイテムを名前の順（アルファベット順）に並べる
byName :: [Item] -> [Item]
byName items = sortOn name items
-- ANCHOR_END: byName

-- ANCHOR: countRareOrBetter
-- ✨ How many items are Rare or rarer?
-- Rare 以上のアイテムはいくつ？
countRareOrBetter :: [Item] -> Int
countRareOrBetter items = length (filter (\i -> rarity i >= Rare) items)
-- ANCHOR_END: countRareOrBetter
