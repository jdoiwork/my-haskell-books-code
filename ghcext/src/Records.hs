-- ANCHOR: pragmas
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- ANCHOR_END: pragmas
-- ANCHOR: all
-- | Records: the same field name in two types, and reading with dots.
-- レコード：2つの型に同じフィールド名、そしてドットで読む
module Records
  ( Hero (..)
  , Monster (..)
  , lambda
  , slime
  , battleLine
  , heal
  ) where

-- ANCHOR: dup
-- 🦸👾 Both have a name and hp; DuplicateRecordFields allows it.
-- どちらにも name と hp がある。DuplicateRecordFields が許してくれる
data Hero = Hero { name :: String, hp :: Int, level :: Int }
  deriving (Show, Eq)

data Monster = Monster { name :: String, hp :: Int }
  deriving (Show, Eq)

lambda :: Hero
lambda = Hero { name = "Lambda", hp = 30, level = 5 }

slime :: Monster
slime = Monster { name = "Slime", hp = 8 }

-- 👀 OverloadedRecordDot: h.name and m.name, without a clash.
-- OverloadedRecordDot：h.name と m.name。ぶつからない
battleLine :: Hero -> Monster -> String
battleLine h m = h.name ++ " (" ++ show h.hp ++ ") vs "
              ++ m.name ++ " (" ++ show m.hp ++ ")"
-- ANCHOR_END: dup

-- ANCHOR: puns
-- 🩹 RecordWildCards: Hero{..} brings every field in as a name.
-- RecordWildCards：Hero{..} で、全部のフィールドが名前として使える
heal :: Hero -> String
heal Hero{..} = name ++ " heals to " ++ show (hp + level * 2)
-- ANCHOR_END: puns
-- ANCHOR_END: all
