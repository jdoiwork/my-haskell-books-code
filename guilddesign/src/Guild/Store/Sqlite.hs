-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultilineStrings #-}

-- 🪶 A store in SQLite.
-- SQLite の置き場
module Guild.Store.Sqlite (withSqliteStore) where

import Database.SQLite.Simple
import Control.Exception (try, throwIO)
import Data.Text         (Text)

import Guild.Roster
import Guild.Store
-- ANCHOR_END: imports

-- ANCHOR: sqlite
-- 🪶 Open the SQLite file, make the table if needed, and lend the store.
-- SQLite のファイルを開き、なければ表を作って、置き場を貸す
withSqliteStore :: FilePath -> (RosterStore -> IO a) -> IO a
withSqliteStore path action =
  withConnection path $ \conn -> do
    execute_ conn """
      CREATE TABLE IF NOT EXISTS adventurers
        ( id    INTEGER PRIMARY KEY
        , name  TEXT    NOT NULL UNIQUE
        , job   TEXT    NOT NULL
        , level INTEGER NOT NULL
        )
      """
    action RosterStore
      { addAdventurer    = add conn
      , listAdventurers  = map toAdventurer <$> query_ conn
          "SELECT id, name, job, level FROM adventurers ORDER BY id"
      , findAdventurer   = \who -> fmap toAdventurer . firstRow <$> query conn
          "SELECT id, name, job, level FROM adventurers WHERE name = ?"
          (Only who)
      , levelUp          = \who -> changed conn
          "UPDATE adventurers SET level = level + 1 WHERE name = ?" who
      , removeAdventurer = \who -> changed conn
          "DELETE FROM adventurers WHERE name = ?" who
      }

-- ➕ SQLite tells a taken name with the error ErrorConstraint.
-- SQLite は、使われている名前を ErrorConstraint のエラーで知らせる
add :: Connection -> NewAdventurer -> IO (Either AddError Int)
add conn new = do
  result <- try (execute conn
    "INSERT INTO adventurers (name, job, level) VALUES (?, ?, ?)"
    (new.newName, new.newJob, new.newLevel))
  case result of
    Right () -> Right . fromIntegral <$> lastInsertRowId conn
    Left (e :: SQLError)
      | e.sqlError == ErrorConstraint -> pure (Left (NameTaken new.newName))
      | otherwise                     -> throwIO e

-- 🔁 Run an UPDATE or DELETE. Did a row change?
-- UPDATE か DELETE を動かす。行が変わった？
changed :: Connection -> Query -> Text -> IO Bool
changed conn sql who = do
  execute conn sql (Only who)
  n <- changes conn
  pure (n > 0)
-- ANCHOR_END: sqlite

-- 🧙 A row as a tuple, to an Adventurer.
-- タプルの行を、Adventurer に
toAdventurer :: (Int, Text, Text, Int) -> Adventurer
toAdventurer (i, n, j, l) = Adventurer i n j l

firstRow :: [a] -> Maybe a
firstRow (x : _) = Just x
firstRow []      = Nothing
-- ANCHOR_END: all
