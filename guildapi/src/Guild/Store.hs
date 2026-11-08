-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- 🔌 Where the roster lives: a record of functions (as in the
--    DB-independent design chapter), in memory or in SQLite.
-- 名簿の置き場：関数のレコード（DB に縛られない設計の章と同じ）。
-- メモリーか SQLite
module Guild.Store
  ( RosterStore (..)
  , newMemoryStore
  , withSqliteStore
  ) where

import Control.Exception      (try, throwIO)
import Data.IORef
import Data.List              (sortOn)
import Data.Map.Strict        qualified as Map
import Data.Text              (Text)
import Database.SQLite.Simple

import Guild.Types
-- ANCHOR_END: imports

-- ANCHOR: store
-- 🔌 What a roster store can do.
-- 名簿の置き場ができること
data RosterStore = RosterStore
  { listAdventurers :: IO [Adventurer]
  , findAdventurer  :: Text -> IO (Maybe Adventurer)
  , addAdventurer   :: NewAdventurer -> IO (Maybe Adventurer)
  , levelUp         :: Text -> IO (Maybe Adventurer)
  }
-- ANCHOR_END: store

-- 🧠 A store in memory (for tests, and for trying things out).
-- メモリーの置き場（テストや、お試しに）
newMemoryStore :: IO RosterStore
newMemoryStore = do
  box <- newIORef (Map.empty, 1 :: Int)
  let list = sortOn (.adventurerId) . Map.elems . fst <$> readIORef box
      find who = Map.lookup who . fst <$> readIORef box
      add new = atomicModifyIORef' box $ \(m, next) ->
        if Map.member new.name m
          then ((m, next), Nothing)
          else
            let a = Adventurer next new.name new.job new.level
            in  ((Map.insert new.name a m, next + 1), Just a)
      up who = atomicModifyIORef' box $ \(m, next) ->
        case Map.lookup who m of
          Nothing -> ((m, next), Nothing)
          Just a  ->
            let a' = Adventurer a.adventurerId a.name a.job (a.level + 1)
            in  ((Map.insert who a' m, next), Just a')
  pure (RosterStore list find add up)

-- 🪶 A store in an SQLite file.
-- SQLite のファイルの置き場
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
    let select = "SELECT id, name, job, level FROM adventurers"
        toAdv (i, n, j, l) = Adventurer i n j l
        find who = fmap toAdv . firstRow <$>
          query conn (select <> " WHERE name = ?") (Only who)
        add new = do
          result <- try (execute conn
            "INSERT INTO adventurers (name, job, level) VALUES (?, ?, ?)"
            (new.name, new.job, new.level))
          case result of
            Right () -> find new.name
            Left (e :: SQLError)
              | e.sqlError == ErrorConstraint -> pure Nothing
              | otherwise                     -> throwIO e
        up who = do
          execute conn
            "UPDATE adventurers SET level = level + 1 WHERE name = ?"
            (Only who)
          find who
    action RosterStore
      { listAdventurers = map toAdv <$> query_ conn (select <> " ORDER BY id")
      , findAdventurer  = find
      , addAdventurer   = add
      , levelUp         = up
      }
  where
    firstRow (x : _) = Just x
    firstRow []      = Nothing
-- ANCHOR_END: all
