-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

-- ANCHOR: module
-- 👾 The monsters in the bestiary, and how they look in JSON.
-- 図鑑のモンスターと、その JSON での形
module Mondex.Monster
  ( Element (..)
  , Monster (..)
  , allElements
  , parseElement
  ) where

-- JSON and the shape of a type.
-- JSON と、型の形
import Data.Aeson   (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- Characters and text.
-- 文字とテキスト
import Data.Char (toLower)
import Data.Text (Text)
-- ANCHOR_END: module

-- ANCHOR: types
-- 🔥 The elements.
-- 属性
data Element = Fire | Water | Grass
  deriving stock    (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 👾 A monster in the bestiary.
-- 図鑑のモンスター
data Monster = Monster
  { name    :: Text
  , hp      :: Int
  , element :: Element
  , drops   :: [Text]
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
-- ANCHOR_END: types

-- ANCHOR: parseElement
-- 📋 All the elements, in order.
-- すべての属性（順番どおり）
allElements :: [Element]
allElements = [minBound .. maxBound]

-- 🔥 Read an element from a word typed by a person.
--    Upper or lower case does not matter: "fire", "Fire", "FIRE".
-- 人が打った言葉から、属性を読む
-- 大文字でも小文字でもよい（"fire"、"Fire"、"FIRE"）
parseElement :: String -> Maybe Element
parseElement word =
  let lower = map toLower word
      match e = map toLower (show e) == lower
  in  case filter match allElements of
        e : _ -> Just e
        []    -> Nothing
-- ANCHOR_END: parseElement
-- ANCHOR_END: all
