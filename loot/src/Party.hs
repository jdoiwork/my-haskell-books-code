-- ANCHOR: all
-- Let us write m.level to read a field.
-- m.level のように書いてフィールドを読めるようにする
{-# LANGUAGE OverloadedRecordDot #-}

-- ANCHOR: module
-- 🧑‍🤝‍🧑 The party: members and their levels.
-- パーティー：メンバーとレベル
module Party
  ( Member (..)
  , rankByLevel
  ) where

-- Sorting.
-- 並べ替え
import Data.List (sortOn)
import Data.Ord  (Down (..))
-- ANCHOR_END: module

-- ANCHOR: member
-- 🧑 A party member.
-- パーティーのメンバー
data Member = Member
  { name  :: String
  , level :: Int
  }
  deriving (Show, Eq, Ord)
-- ANCHOR_END: member

-- ANCHOR: rank
-- 🏆 Strongest first: sort by level, highest first.
-- 強い順：レベルの高い順に並べる
rankByLevel :: [Member] -> [Member]
rankByLevel members = sortOn (\m -> Down m.level) members
-- ANCHOR_END: rank
-- ANCHOR_END: all
