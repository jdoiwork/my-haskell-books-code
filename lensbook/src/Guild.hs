-- ANCHOR: pragmas
{-# LANGUAGE TemplateHaskell #-}
-- ANCHOR_END: pragmas
-- ANCHOR: all
-- | The same party, with lenses made by makeLenses.
-- 同じパーティーを、makeLenses で作ったレンズで
module Guild
  ( Stats (..)
  , Hero (..)
  , Party (..)
  , hp, mp, level, name, stats, bag, leader, members
  , party
  , knockOutLeader
  , levelUpAll
  , memberNames
  , givePotion
  , potionsOf
  , firstMemberName
  , totalHp
  ) where

-- ANCHOR: imports
import Control.Lens
import qualified Data.Map.Strict as Map
-- ANCHOR_END: imports

-- ANCHOR: types
-- 📊 Fields start with _; makeLenses makes a lens for each,
--    named without the _.
-- フィールド名は _ で始める。makeLenses が、_ を取った名前で
-- フィールドごとにレンズを作る
data Stats = Stats { _hp :: Int, _mp :: Int, _level :: Int }
  deriving (Show, Eq)

data Hero = Hero
  { _name  :: String
  , _stats :: Stats
  , _bag   :: Map.Map String Int
  }
  deriving (Show, Eq)

data Party = Party { _leader :: Hero, _members :: [Hero] }
  deriving (Show, Eq)

makeLenses ''Stats
makeLenses ''Hero
makeLenses ''Party

party :: Party
party = Party
  { _leader  = Hero "Lambda" (Stats 30 10 5) (Map.fromList [("herb", 2)])
  , _members =
      [ Hero "Python" (Stats 25 20 4) (Map.fromList [("potion", 1)])
      , Hero "Rust"   (Stats 40 5 6)  Map.empty
      ]
  }
-- ANCHOR_END: types

-- ANCHOR: write
-- 🎯 Changing two levels down: just say the path.
-- 2段下を変える：道順を言うだけ
knockOutLeader :: Party -> Party
knockOutLeader = set (leader . stats . hp) 0
-- ANCHOR_END: write

-- ANCHOR: traversed
-- ⬆️ Every member gains a level: traversed walks into the list.
-- 仲間全員のレベルを上げる：traversed がリストの中へ入っていく
levelUpAll :: Party -> Party
levelUpAll = over (members . traversed . stats . level) (+ 1)

-- 📋 Read every member's name.
-- 仲間全員の名前を読む
memberNames :: Party -> [String]
memberNames = toListOf (members . traversed . name)
-- ANCHOR_END: traversed

-- ANCHOR: maps
-- 🧪 Give the leader a potion: at looks inside a Map.
-- リーダーにポーションを1つ渡す：at で Map の中をのぞく
givePotion :: Party -> Party
givePotion = over (leader . bag . at "potion") (Just . maybe 1 (+ 1))

-- 🔢 How many potions does a hero have? (0 if none)
-- 勇者は、ポーションを何個持っている？（なければ 0）
potionsOf :: Hero -> Int
potionsOf hero = hero ^. bag . at "potion" . non 0

-- 🔍 The first member's name, if there is a first member.
-- 1人目の仲間の名前。1人目がいれば
firstMemberName :: Party -> Maybe String
firstMemberName p = p ^? members . ix 0 . name

-- ➕ The sum of everybody's HP.
-- 全員の HP の合計
totalHp :: Party -> Int
totalHp p = sumOf (leader . stats . hp) p
          + sumOf (members . traversed . stats . hp) p
-- ANCHOR_END: maps
-- ANCHOR_END: all
