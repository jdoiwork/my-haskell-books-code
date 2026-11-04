-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}

-- 🐘 A tour of the roster on PostgreSQL.
-- PostgreSQL の名簿を、ひととおり動かす
module Main (main) where

import Database.PostgreSQL.Simple
import Data.ByteString.Char8    qualified as B
import Data.Pool                (withResource, destroyAllResources)
import Control.Concurrent.Async (mapConcurrently_)
import System.Environment       (lookupEnv)
import Data.Maybe               (fromMaybe)

import PgRoster
import Migrate
import GuildPool

-- ANCHOR: url
-- 🔌 Where the database is: GUILD_DB, or the container on this machine.
-- データベースの場所：GUILD_DB か、この Mac のコンテナ
databaseUrl :: IO String
databaseUrl = do
  fromEnv <- lookupEnv "GUILD_DB"
  pure (fromMaybe localDb fromEnv)
  where
    localDb =
      "postgresql://postgres:guild@localhost:5432/guild"
-- ANCHOR_END: url

main :: IO ()
main = do
  url <- databaseUrl
  -- ANCHOR: tour
  conn <- connectPostgreSQL (B.pack url)
  _ <- execute_ conn "SET client_min_messages TO WARNING"
  _ <- execute_ conn "DROP TABLE IF EXISTS adventurers, schema_migrations"
  result <- migrate conn "migrations"
  print result
  appliedScripts conn >>= mapM_ putStrLn
  lambdaId <- addAdventurer conn (NewAdventurer "Lambda" "Mage" 3)
  pythonId <- addAdventurer conn (NewAdventurer "Python" "Archer" 5)
  print (lambdaId, pythonId)
  levelUp conn "Lambda" >>= print
  listAdventurers conn >>= mapM_ print
  -- ANCHOR_END: tour
  -- ANCHOR: poolTour
  pool <- newGuildPool (B.pack url)
  mapConcurrently_ (\_ -> withResource pool (\c -> levelUp c "Lambda"))
                   [1 .. 100 :: Int]
  findAdventurer conn "Lambda" >>= print
  [Only used] <- query_ conn
    "SELECT count(*) FROM pg_stat_activity \
    \WHERE datname = current_database() AND pid <> pg_backend_pid()"
  print (used :: Int)
  destroyAllResources pool
  -- ANCHOR_END: poolTour
  close conn
-- ANCHOR_END: all
