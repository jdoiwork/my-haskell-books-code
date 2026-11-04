-- ANCHOR: all
-- | Passing the connection and the logger by hand, to every function.
-- 接続とロガーを、手で、全部の関数に渡す
module App.ByHand
  ( setupH
  , findByNameH
  , addAdventurerH
  , enlistH
  ) where

import Data.Text (Text)
import Database.SQLite.Simple

import App.Types

-- ANCHOR: byHand
-- 🗃️ Make the table.
-- 表を作る
setupH :: Connection -> IO ()
setupH conn = execute_ conn
  "CREATE TABLE adventurers (id INTEGER PRIMARY KEY, \
  \name TEXT UNIQUE, job TEXT, level INTEGER)"

-- 🔍 Find by name.
-- 名前で探す
findByNameH :: Connection -> Text -> IO (Maybe Adventurer)
findByNameH conn who = do
  rows <- query conn
    "SELECT id, name, job, level FROM adventurers WHERE name = ?" (Only who)
  pure $ case rows of
    (i, n, j, l) : _ -> Just (Adventurer i n j l)
    []               -> Nothing

-- ➕ Add one. Needs the connection, and the logger too.
-- 1人足す。接続も、ロガーも要る
addAdventurerH :: Connection -> (Text -> IO ()) -> NewAdventurer -> IO Int
addAdventurerH conn logLine new = do
  execute conn "INSERT INTO adventurers (name, job, level) VALUES (?, ?, ?)"
    (newName new, newJob new, newLevel new)
  logLine ("added " <> newName new)
  fromIntegral <$> lastInsertRowId conn

-- 📝 Enlist: passes conn and logLine on, without using them itself.
-- 登録：conn と logLine を、自分では使わずに、次へ渡すだけ
enlistH :: Connection -> (Text -> IO ())
        -> NewAdventurer -> IO (Either AddError Int)
enlistH conn logLine new = do
  found <- findByNameH conn (newName new)
  case found of
    Just _  -> pure (Left (NameTaken (newName new)))
    Nothing -> Right <$> addAdventurerH conn logLine new
-- ANCHOR_END: byHand
-- ANCHOR_END: all
