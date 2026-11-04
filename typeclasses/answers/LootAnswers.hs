{-# LANGUAGE OverloadedStrings #-}
module LootAnswers where

import Loot

-- ANCHOR: element
-- 🔥 The elements, from the data modeling chapter.
-- データをモデリングする章の属性
data Element = Fire | Water | Grass
  deriving (Show, Eq)

instance Describable Element where
  describe Fire  = "Hot!"
  describe Water = "Wet!"
  describe Grass = "Green!"
-- ANCHOR_END: element

-- ANCHOR: totalLoot
-- 💰 Put all the loot together.
-- 戦利品をすべてまとめる
totalLoot :: [Loot] -> Loot
totalLoot loots = mconcat loots
-- ANCHOR_END: totalLoot

-- ANCHOR: best
-- 🥇 The best score. Putting two together keeps the higher one.
-- 最高記録。2つをまとめると、高いほうが残る
newtype Best = Best Int
  deriving (Show, Eq)

instance Semigroup Best where
  Best a <> Best b = Best (max a b)
-- ANCHOR_END: best
