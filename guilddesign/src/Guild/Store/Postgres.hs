-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultilineStrings #-}

-- 🐘 A store in PostgreSQL.
-- PostgreSQL の置き場
module Guild.Store.Postgres (withPostgresStore) where

import Database.PostgreSQL.Simple
import Control.Exception (bracket, try, throwIO)
import Data.ByteString   (ByteString)
import Data.Text         (Text)

import Guild.Roster
import Guild.Store
-- ANCHOR_END: imports

-- ANCHOR: postgres
-- 🐘 Connect, make the table if needed, and lend the store.
-- 接続して、なければ表を作って、置き場を貸す
withPostgresStore :: ByteString -> (RosterStore -> IO a) -> IO a
withPostgresStore url action =
  bracket (connectPostgreSQL url) close $ \conn -> do
    _ <- execute_ conn "SET client_min_messages TO WARNING"
    _ <- execute_ conn """
      CREATE TABLE IF NOT EXISTS adventurers
        ( id    INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY
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
      , levelUp          = \who -> (> 0) <$> execute conn
          "UPDATE adventurers SET level = level + 1 WHERE name = ?" (Only who)
      , removeAdventurer = \who -> (> 0) <$> execute conn
          "DELETE FROM adventurers WHERE name = ?" (Only who)
      }

-- ➕ PostgreSQL tells a taken name with the code 23505.
-- PostgreSQL は、使われている名前をコード 23505 で知らせる
add :: Connection -> NewAdventurer -> IO (Either AddError Int)
add conn new = do
  result <- try (query conn
    "INSERT INTO adventurers (name, job, level) VALUES (?, ?, ?) \
    \RETURNING id"
    (new.newName, new.newJob, new.newLevel))
  case result of
    Right [Only newId] -> pure (Right newId)
    Right _            -> fail "INSERT ... RETURNING gave no id"
    Left (e :: SqlError)
      | e.sqlState == "23505" -> pure (Left (NameTaken new.newName))
      | otherwise             -> throwIO e

-- 🧙 A row as a tuple, to an Adventurer.
-- タプルの行を、Adventurer に
toAdventurer :: (Int, Text, Text, Int) -> Adventurer
toAdventurer (i, n, j, l) = Adventurer i n j l

-- 🥇 The first row, if any.
-- 最初の行（あれば）
firstRow :: [a] -> Maybe a
firstRow (x : _) = Just x
firstRow []      = Nothing
-- ANCHOR_END: postgres
-- ANCHOR_END: all
