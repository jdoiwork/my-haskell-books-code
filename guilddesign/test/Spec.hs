-- ANCHOR: all
-- ANCHOR: module
{-# LANGUAGE OverloadedStrings #-}

-- 🧪 The contract, on all three stores.
-- 契約のテストを、3つの置き場で
module Main (main) where

import Contract
import DesignAnswers
import Guild.Store
import Guild.Store.Memory
import Guild.Store.Sqlite
import Guild.Store.Postgres

import Database.PostgreSQL.Simple (connectPostgreSQL, execute_, close)
import Data.ByteString            (ByteString)
import Data.Text                  qualified as T
import Data.Text.Encoding         (encodeUtf8)
import Data.Text.Lazy             qualified as LT
import TestContainers.Hspec
import Test.Hspec
-- ANCHOR_END: module

-- ANCHOR: spec
main :: IO ()
main = hspec $ do
  describe "memory" $
    around withMemory rosterContract

  describe "SQLite" $
    around (withSqliteStore ":memory:") rosterContract

  describe "PostgreSQL" $
    aroundAll (withContainers postgres) $
      aroundWith withFreshPostgres rosterContract

  -- answers for the training / トレーニングの解答
  describe "answers" $
    around withMemory extraContract

-- 🧠 A new, empty store in memory.
-- メモリーに、空っぽの置き場を新しく
withMemory :: (RosterStore -> IO ()) -> IO ()
withMemory action = newMemoryStore >>= action
-- ANCHOR_END: spec

-- ANCHOR: postgres
-- 🐘 An empty table each time: drop it, and withPostgresStore makes it.
-- 毎回、空っぽの表：消しておけば、withPostgresStore が作り直す
withFreshPostgres :: (RosterStore -> IO ()) -> ByteString -> IO ()
withFreshPostgres action url = do
  conn <- connectPostgreSQL url
  _ <- execute_ conn "SET client_min_messages TO WARNING"
  _ <- execute_ conn "DROP TABLE IF EXISTS adventurers"
  close conn
  withPostgresStore url action

-- 🐳 PostgreSQL 18 in a container (as in the PostgreSQL chapter).
-- コンテナの PostgreSQL 18（PostgreSQL の章と同じ）
postgres :: TestContainer ByteString
postgres = do
  container <- run (containerRequest (fromTag "postgres:18")
    & setEnv [("POSTGRES_PASSWORD", "guild"), ("POSTGRES_DB", "guild")]
    & setExpose ["5432/tcp"]
    & setWaitingFor ready)
  let (host, port) = containerAddress container "5432/tcp"
  pure (encodeUtf8 (T.unwords
    [ "host=" <> host
    , "port=" <> T.pack (show port)
    , "user=postgres password=guild dbname=guild"
    ]))
  where
    ready     = waitUntilTimeout 60 (setupDone <> portOpen)
    setupDone = waitForLogLine Stdout ("init process complete" `LT.isInfixOf`)
    portOpen  = waitUntilMappedPortReachable "5432/tcp"
-- ANCHOR_END: postgres
-- ANCHOR_END: all
