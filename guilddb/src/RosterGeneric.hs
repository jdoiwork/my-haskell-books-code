-- ANCHOR: all
-- ANCHOR: pragma
-- Let us write "..." as Text and SQL, and derive with anyclass.
-- "..." を Text や SQL として書く。anyclass で資格を自動で作る
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
-- ANCHOR_END: pragma

-- 🤖 The same types as Roster, with FromRow and ToRow made by Generic.
-- Roster と同じ型。FromRow と ToRow を Generic で自動で作る
module RosterGeneric
  ( Adventurer (..)
  , NewAdventurer (..)
  ) where

-- ANCHOR: imports
import Database.SQLite.Simple (FromRow, ToRow)
import Data.Text              (Text)
import GHC.Generics           (Generic)
-- ANCHOR_END: imports

-- ANCHOR: types
-- 🧙 An adventurer, as read from the table.
-- 表から読んだ冒険者
data Adventurer = Adventurer
  { adventurerId :: Int
  , name         :: Text
  , job          :: Text
  , level        :: Int
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (FromRow)

-- ✍️ A new adventurer, before the database gives it an id.
-- 新しい冒険者（データベースが id を付ける前）
data NewAdventurer = NewAdventurer
  { newName  :: Text
  , newJob   :: Text
  , newLevel :: Int
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToRow)
-- ANCHOR_END: types
-- ANCHOR_END: all
