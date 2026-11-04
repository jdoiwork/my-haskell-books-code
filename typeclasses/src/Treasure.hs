{-# LANGUAGE OverloadedRecordDot #-}
-- ANCHOR: module
module Treasure where

-- sortOn: sort by a key (we saw it in the toolbox chapter).
-- sortOn：キーを基準に並べ替える（道具箱の章で見た）
import Data.List (sortOn)

-- Map: a dictionary (we saw it in the Map chapter).
-- Map：辞書（Map の章で見た）
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
-- ANCHOR_END: module

-- ANCHOR: rarity
-- 💎 How rare an item is, from the most common to the rarest.
-- アイテムのレア度。ありふれたものから、いちばん珍しいものの順
data Rarity = Common | Rare | Epic | Legendary
  deriving (Show, Eq, Ord)
-- ANCHOR_END: rarity

-- ANCHOR: item
-- 🎁 An item dropped by a monster.
-- モンスターが落としたアイテム
data Item = Item
  { name   :: String
  , rarity :: Rarity
  }
  deriving (Show, Eq)
-- ANCHOR_END: item

-- ANCHOR: drops
-- 🎁 What the monsters dropped today.
-- 今日モンスターが落としたもの
drops :: [Item]
drops =
  [ Item { name = "herb",        rarity = Common    }
  , Item { name = "dragon fang", rarity = Legendary }
  , Item { name = "silver ring", rarity = Rare      }
  , Item { name = "potion",      rarity = Common    }
  , Item { name = "magic staff", rarity = Epic      }
  ]
-- ANCHOR_END: drops

-- ANCHOR: rarest
-- 🏆 Sort the items from the rarest to the most common.
--    This works because Rarity is an instance of Ord.
-- いちばん珍しいものから順に並べる
-- Rarity が Ord のインスタンスだからできる
rarestFirst :: [Item] -> [Item]
rarestFirst items = reverse (sortOn rarity items)
-- ANCHOR_END: rarest

-- ANCHOR: countByRarity
-- 📊 How many items of each rarity?
--    Rarity can be a Map key because it is an instance of Ord.
-- レア度ごとに、アイテムはいくつ？
-- Rarity が Ord のインスタンスなので、Map のキーにできる
countByRarity :: [Item] -> Map Rarity Int
countByRarity items =
  Map.fromListWith (+) [ (item.rarity, 1) | item <- items ]
-- ANCHOR_END: countByRarity

-- ANCHOR: gold
-- 🪙 Gold, shown with a "G" at the end.
-- 末尾に「G」を付けて表示するゴールド
newtype Gold = Gold Int
  deriving (Eq, Ord)

instance Show Gold where
  show (Gold g) = show g ++ " G"
-- ANCHOR_END: gold
