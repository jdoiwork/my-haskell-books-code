-- ANCHOR: all
-- ANCHOR: module
{-# LANGUAGE OverloadedRecordDot #-}
module Appraisal where

-- sortOn and Down: sort by a key, largest first.
-- sortOn と Down：キーで並べ替える（大きい順）
import Data.List (sortOn)
import Data.Ord  (Down (..))

-- Map: a dictionary.
-- Map：辞書
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- readMaybe: read a value, or Nothing if we cannot.
-- readMaybe：値として読む。読めなければ Nothing
import Text.Read (readMaybe)
-- ANCHOR_END: module

-- ANCHOR: types
-- 💎 How rare an item is, from the most common to the rarest.
-- アイテムのレア度。ありふれたものから、いちばん珍しいものの順
data Rarity = Common | Rare | Epic | Legendary
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- 🎁 One piece of loot: who found what, and how rare it is.
-- 戦利品1つ：誰が、何を見つけたか。そのレア度
data Loot = Loot
  { hero   :: String
  , item   :: String
  , rarity :: Rarity
  }
  deriving (Show, Eq)
-- ANCHOR_END: types

-- ANCHOR: parse
-- ✂️ Split a string at every separator: splitOn ',' "a,b" == ["a","b"]
-- 区切り文字のところで、文字列を分ける
splitOn :: Char -> String -> [String]
splitOn sep text =
  case break (== sep) text of
    (before, [])       -> [before]
    (before, _ : rest) -> before : splitOn sep rest

-- 🔍 Read one line "Lambda,dragon fang,Legendary" as a Loot.
--    A broken line gives Nothing.
-- 1行 "Lambda,dragon fang,Legendary" を Loot として読む
-- 壊れた行なら Nothing
parseLoot :: String -> Maybe Loot
parseLoot line =
  case splitOn ',' line of
    [who, what, rarityText] ->
      case readMaybe rarityText of
        Just r  -> Just (Loot { hero = who, item = what, rarity = r })
        Nothing -> Nothing
    _ -> Nothing
-- ANCHOR_END: parse

-- ANCHOR: count
-- 📊 Count the loot for each rarity.
-- レア度ごとに、戦利品を数える
countByRarity :: [Loot] -> Map Rarity Int
countByRarity loots =
  Map.fromListWith (+) [ (loot.rarity, 1) | loot <- loots ]

-- 📜 One line per rarity, from Common to Legendary.
--    A rarity nobody found is shown as 0.
-- レア度ごとに1行。Common から Legendary の順
-- 誰も見つけていないレア度は 0 と表示する
reportLines :: Map Rarity Int -> [String]
reportLines counts =
  [ show r ++ ": " ++ show (Map.findWithDefault 0 r counts)
  | r <- [minBound .. maxBound]
  ]
-- ANCHOR_END: count

-- ANCHOR: rarest
-- 🏆 The rarest loot. If there is no loot at all, Nothing.
-- いちばんレアな戦利品。戦利品が1つもなければ Nothing
rarest :: [Loot] -> Maybe Loot
rarest loots =
  case sortOn (\loot -> Down loot.rarity) loots of
    []       -> Nothing
    best : _ -> Just best
-- ANCHOR_END: rarest
-- ANCHOR_END: all
