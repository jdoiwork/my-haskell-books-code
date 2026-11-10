-- ANCHOR: all
-- ANCHOR: imports
-- | The roster in the App monad: no conn, no logger in the arguments.
-- App のモナドで書いた名簿：引数に、conn もロガーもない
module App.Roster
  ( setup
  , findByName
  , addAdventurer
  , listAll
  ) where

import Data.Text (Text)
import Database.SQLite.Simple

import App.Env
import App.Types
-- ANCHOR_END: imports

-- ANCHOR: roster
-- 🗃️ Make the table.
-- 表を作る
setup :: App ()
setup = db $ \conn -> execute_ conn
  "CREATE TABLE adventurers (id INTEGER PRIMARY KEY, \
  \name TEXT UNIQUE, job TEXT, level INTEGER)"

-- ANCHOR: findByName
-- 🔍 Find by name.
-- 名前で探す
findByName :: Text -> App (Maybe Adventurer)
findByName who = do
  rows <- db $ \conn -> query conn
    "SELECT id, name, job, level FROM adventurers WHERE name = ?" (Only who)
  pure $ case rows of
    (i, n, j, l) : _ -> Just (Adventurer i n j l)
    []               -> Nothing
-- ANCHOR_END: findByName

-- ➕ Add one, and say so in the log.
-- 1人足して、ログに書く
addAdventurer :: NewAdventurer -> App Int
addAdventurer new = do
  newId <- db $ \conn -> do
    execute conn
      "INSERT INTO adventurers (name, job, level) VALUES (?, ?, ?)"
      (newName new, newJob new, newLevel new)
    fromIntegral <$> lastInsertRowId conn
  logInfo ("added " <> newName new)
  pure newId

-- 📋 Everybody, by id.
-- 全員を、番号順に
listAll :: App [Adventurer]
listAll = do
  rows <- db $ \conn -> query_ conn
    "SELECT id, name, job, level FROM adventurers ORDER BY id"
  pure [ Adventurer i n j l | (i, n, j, l) <- rows ]
-- ANCHOR_END: roster
-- ANCHOR_END: all
