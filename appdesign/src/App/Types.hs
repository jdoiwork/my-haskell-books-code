-- ANCHOR: all
-- | The roster's own types. They know nothing about databases.
-- 名簿の型。データベースのことは、何も知らない
module App.Types
  ( Adventurer (..)
  , NewAdventurer (..)
  , AddError (..)
  ) where

import Data.Text (Text)

-- ANCHOR: types
-- 🧙 An adventurer in the roster, and a new one before it has an id.
-- 名簿の冒険者と、番号が付く前の新しい冒険者
data Adventurer = Adventurer
  { advId :: Int, advName :: Text, advJob :: Text, advLevel :: Int }
  deriving (Show, Eq)

data NewAdventurer = NewAdventurer
  { newName :: Text, newJob :: Text, newLevel :: Int }
  deriving (Show, Eq)

-- 🚫 Why an adventurer could not be added.
-- 登録できなかった理由
data AddError
  = NameTaken Text   -- the name is already used / 名前が使われている
  | BadLevel Int     -- the level is out of range / レベルが範囲の外
  | EmptyName        -- no name / 名前がない
  deriving (Show, Eq)
-- ANCHOR_END: types
-- ANCHOR_END: all
