-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- 🧙 The roster's own types. They know nothing about databases.
-- 名簿の型。データベースのことは、何も知らない
module Guild.Roster
  ( Adventurer (..)
  , NewAdventurer (..)
  , formatAdventurer
  ) where

import Data.Text (Text)
import Data.Text qualified as T
-- ANCHOR_END: imports

-- ANCHOR: types
-- 🧙 An adventurer in the roster.
-- 名簿の冒険者
data Adventurer = Adventurer
  { adventurerId :: Int
  , name         :: Text
  , job          :: Text
  , level        :: Int
  }
  deriving (Show, Eq)

-- ✍️ A new adventurer, before the store gives it an id.
-- 新しい冒険者（置き場が id を付ける前）
data NewAdventurer = NewAdventurer
  { newName  :: Text
  , newJob   :: Text
  , newLevel :: Int
  }
  deriving (Show, Eq)
-- ANCHOR_END: types

-- 🖨️ One line for the roster: "Lambda      Mage      Lv 3"
-- 名簿の1行
formatAdventurer :: Adventurer -> Text
formatAdventurer a =
  T.justifyLeft 12 ' ' a.name
    <> T.justifyLeft 10 ' ' a.job
    <> "Lv " <> T.pack (show a.level)
-- ANCHOR_END: all
