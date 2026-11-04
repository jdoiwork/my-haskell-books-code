{-# LANGUAGE OverloadedRecordDot #-}
-- ANCHOR: all
-- | Nested records without lens: reading is easy, changing is not.
-- lens なしの入れ子のレコード：読むのは楽、変えるのは大変
module Plain
  ( Stats (..)
  , Hero (..)
  , Party (..)
  , party
  , leaderHp
  , knockOutLeader
  ) where

-- ANCHOR: types
-- 📊 Numbers of one hero.
-- 勇者1人の数値
data Stats = Stats { hp :: Int, mp :: Int, level :: Int }
  deriving (Show, Eq)

-- 🦸 A hero has a name and stats.
-- 勇者には、名前と数値がある
data Hero = Hero { name :: String, stats :: Stats }
  deriving (Show, Eq)

-- 👥 A party has a leader and members.
-- パーティーには、リーダーと仲間がいる
data Party = Party { leader :: Hero, members :: [Hero] }
  deriving (Show, Eq)

party :: Party
party = Party
  { leader  = Hero "Lambda" (Stats 30 10 5)
  , members = [Hero "Python" (Stats 25 20 4), Hero "Rust" (Stats 40 5 6)]
  }
-- ANCHOR_END: types

-- ANCHOR: read
-- 👀 Reading two levels down: one dot per level.
-- 2段下を読む：1段ごとに1つのドット
leaderHp :: Party -> Int
leaderHp p = p.leader.stats.hp
-- ANCHOR_END: read

-- ANCHOR: write
-- 😵 Changing two levels down: rebuild every level on the way.
-- 2段下を変える：途中の段を、全部作り直す
knockOutLeader :: Party -> Party
knockOutLeader p =
  p { leader = (p.leader) { stats = (p.leader.stats) { hp = 0 } } }
-- ANCHOR_END: write
-- ANCHOR_END: all
