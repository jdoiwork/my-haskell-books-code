-- ANCHOR: all
{-# LANGUAGE DeriveAnyClass #-}

-- ANCHOR: module
-- 👾 The monster bestiary, saved as JSON (from the JSON chapter).
-- JSON で保存するモンスター図鑑（JSON の章と同じ）
module Monster
  ( Element (..)
  , Monster (..)
  ) where

-- JSON and the shape of a type.
-- JSON と、型の形
import Data.Aeson   (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- Text.
-- テキスト
import Data.Text (Text)
-- ANCHOR_END: module

-- ANCHOR: types
-- 🔥 The elements.
-- 属性
data Element = Fire | Water | Grass
  deriving stock    (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
-- ANCHOR_END: types
-- ANCHOR_END: all
