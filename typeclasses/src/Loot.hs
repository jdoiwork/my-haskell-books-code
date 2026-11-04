{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: module
module Loot where

-- sort: sort a list.
-- sort：リストを並べ替える
import Data.List (sort)

-- Text: the string type for text shown to people.
-- Text：人に見せる文章のための文字列の型
import Data.Text (Text)
import Data.Text qualified as T

-- Rarity, Item and Gold, from the type class introduction chapter.
-- 型クラス入門の章で作った Rarity、Item、Gold
import Treasure
-- ANCHOR_END: module

-- ANCHOR: describable
-- 📖 Things that can be described in the bestiary.
--    A type that is an instance of this class must have describe.
-- 図鑑で説明できるもの
-- この型クラスのインスタンスになる型は、describe を持つ
class Describable a where
  describe :: a -> Text
-- ANCHOR_END: describable

-- ANCHOR: instances
-- 📖 How to describe a rarity.
-- レア度の説明のしかた
instance Describable Rarity where
  describe Common    = "You see these everywhere."
  describe Rare      = "Not easy to find."
  describe Epic      = "Heroes talk about these."
  describe Legendary = "Only in old stories!"

-- 📖 How to describe an item.
-- アイテムの説明のしかた
instance Describable Item where
  describe item = T.pack item.name <> " (" <> T.pack (show item.rarity) <> ")"

-- 📖 Even Bool can learn to describe itself.
-- Bool にだって、あとから説明のしかたを教えられる
instance Describable Bool where
  describe True  = "yes, indeed"
  describe False = "no way"
-- ANCHOR_END: instances

-- ANCHOR: announce
-- 📣 Announce anything that can be described.
-- 説明できるものなら、何でも発表する
announce :: Describable a => a -> Text
announce x = "Look! " <> describe x
-- ANCHOR_END: announce

-- ANCHOR: loot
-- 💰 The loot from one battle: gold and item names.
-- 1回の戦闘の戦利品：ゴールドとアイテム名
data Loot = Loot
  { lootGold  :: Int
  , lootItems :: [String]
  }
  deriving (Show, Eq)

-- 🤝 Two loots can be put together into one.
-- 2つの戦利品は、1つにまとめられる
instance Semigroup Loot where
  Loot g1 is1 <> Loot g2 is2 = Loot (g1 + g2) (is1 ++ is2)

-- 🫙 The empty loot: putting it together changes nothing.
-- 空っぽの戦利品：まとめても何も変わらない
instance Monoid Loot where
  mempty = Loot 0 []
-- ANCHOR_END: loot

-- ANCHOR: battles
-- ⚔️ The loot from three battles.
-- 3回の戦闘の戦利品
battles :: [Loot]
battles =
  [ Loot 30 ["herb"]
  , Loot 0  []
  , Loot 50 ["silver ring", "potion"]
  ]
-- ANCHOR_END: battles

-- ANCHOR: showSorted
-- 🔤 Sort a list and turn it into a string.
--    The elements must be sortable (Ord) and showable (Show).
-- リストを並べ替えて、文字列にする
-- 要素は並べられて（Ord）、表示できる（Show）必要がある
showSorted :: (Ord a, Show a) => [a] -> String
showSorted xs = show (sort xs)
-- ANCHOR_END: showSorted

-- ANCHOR: skillsets
-- ⚔️ Skill set: sword skills. Whoever learns it can slash.
-- スキルセット：剣術。身につけた者は、斬撃ができる
class Swordsmanship a where
  slash :: a -> Int

-- ✨ Skill set: holy magic. Whoever learns it can heal.
-- スキルセット：白魔法。身につけた者は、回復ができる
class HolyMagic a where
  heal :: a -> Int

-- 🔥 Skill set: black magic. Whoever learns it can cast
--    two spells: fireballs and blizzards.
-- スキルセット：黒魔法。身につけた者は、
-- 火の玉と吹雪の、2つの魔法を放てる
class BlackMagic a where
  fireball :: a -> Int
  blizzard :: a -> Int
-- ANCHOR_END: skillsets

-- ANCHOR: heroes
-- 🧑 Some heroes. Each one has a level.
-- 勇者たち。それぞれレベルを持っている
newtype Warrior = Warrior Int deriving Show
newtype Priest  = Priest Int  deriving Show
newtype Wizard  = Wizard Int  deriving Show
newtype Paladin = Paladin Int deriving Show
-- ANCHOR_END: heroes

-- ANCHOR: learn
-- 📜 Who has learned which skill set.
-- 誰が、どのスキルセットを身につけたか
instance Swordsmanship Warrior where
  slash (Warrior lv) = lv * 12

instance HolyMagic Priest where
  heal (Priest lv) = lv * 10

instance BlackMagic Wizard where
  fireball (Wizard lv) = lv * 15
  blizzard (Wizard lv) = lv * 12

-- A paladin has learned two skill sets.
-- 聖騎士は、2つのスキルセットを身につけている
instance Swordsmanship Paladin where
  slash (Paladin lv) = lv * 8

instance HolyMagic Paladin where
  heal (Paladin lv) = lv * 6
-- ANCHOR_END: learn

-- ANCHOR: skillUsers
-- ⚔️ Anyone with sword skills can do a double slash.
-- 剣術を持つ者なら誰でも、二段斬りができる
doubleSlash :: Swordsmanship a => a -> Int
doubleSlash hero = slash hero * 2

-- 🌟 A holy slash needs BOTH sword skills and holy magic.
-- 聖なる斬撃には、剣術と白魔法の「両方」がいる
holySlash :: (Swordsmanship a, HolyMagic a) => a -> Int
holySlash hero = slash hero + heal hero
-- ANCHOR_END: skillUsers
