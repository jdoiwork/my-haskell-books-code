-- ANCHOR: all
-- 🔌 What a store of the roster can do: a record of functions.
-- 名簿の置き場ができること：関数を並べたレコード
module Guild.Store
  ( RosterStore (..)
  , AddError (..)
  ) where

import Data.Text   (Text)
import Guild.Roster

-- ANCHOR: store
-- 🔌 The roster's store. Each field is one thing it can do.
-- 名簿の置き場。フィールドの1つ1つが、できることの1つ
data RosterStore = RosterStore
  { addAdventurer    :: NewAdventurer -> IO (Either AddError Int)
  , listAdventurers  :: IO [Adventurer]
  , findAdventurer   :: Text -> IO (Maybe Adventurer)
  , levelUp          :: Text -> IO Bool
  , removeAdventurer :: Text -> IO Bool
  }

-- 🚫 Why an adventurer could not be added.
-- 登録できなかった理由
data AddError
  = NameTaken Text   -- the name is already in the roster / 名前が使われている
  deriving (Show, Eq)
-- ANCHOR_END: store
-- ANCHOR_END: all
