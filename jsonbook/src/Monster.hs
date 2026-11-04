-- ANCHOR: all
-- ANCHOR: pragma
-- Let us write Text values as "Slime".
-- "Slime" のように Text の値を書けるようにする
{-# LANGUAGE OverloadedStrings #-}
-- Let us write deriving anyclass (ToJSON, FromJSON).
-- deriving anyclass (ToJSON, FromJSON) と書けるようにする
{-# LANGUAGE DeriveAnyClass #-}
-- ANCHOR_END: pragma

-- ANCHOR: module
module Monster where

-- The JSON classes, and Generic that describes the shape of a type.
-- JSON の型クラスと、型の形を教える Generic
import Data.Aeson   (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- Text.
-- テキスト
import Data.Text (Text)
-- ANCHOR_END: module

-- ANCHOR: element
-- 🔥 The elements.
-- 属性
data Element = Fire | Water | Grass
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
-- ANCHOR_END: element

-- ANCHOR: monster
-- 👾 A monster in the bestiary.
-- モンスター図鑑のモンスター
data Monster = Monster
  { name    :: Text
  , hp      :: Int
  , element :: Element
  , drops   :: [Text]
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
-- ANCHOR_END: monster

-- ANCHOR: monsters
-- 👾 Some monsters.
-- モンスターたち
slime :: Monster
slime = Monster
  { name    = "Slime"
  , hp      = 30
  , element = Water
  , drops   = ["jelly"]
  }

goblin :: Monster
goblin = Monster
  { name    = "Goblin"
  , hp      = 50
  , element = Grass
  , drops   = ["club", "copper coin"]
  }
-- ANCHOR_END: monsters
-- ANCHOR_END: all
