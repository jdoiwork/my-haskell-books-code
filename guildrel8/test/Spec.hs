-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}

-- 🧪 Tests of the hasql and rel8 roster, on PostgreSQL in a container.
-- hasql と rel8 の名簿のテスト（コンテナの PostgreSQL で）
module Main (main) where

import Data.Text      qualified as T
import Data.Text.Lazy qualified as LT
import Hasql.Connection (Connection, release)
import Hasql.Session    (sql, statement)
import Control.Exception (bracket)
import Rel8 qualified
import TestContainers.Hspec
import Test.Hspec

import HasqlRoster
import Rel8Roster qualified as R

-- ANCHOR: spec
main :: IO ()
main = hspec $
  aroundAll (withContainers postgres) $
    aroundWith withFreshGuild $ do

      describe "hasql" $
        -- 「レベルの高い順に、名前とレベルを返す」
        it "lists the strong ones, strongest first" $ \conn -> do
          addThree conn
          runSession conn (statement 4 strong)
            `shouldReturn` [("Merlin", 7), ("Python", 5)]

      describe "rel8" $ do
        -- 「依頼板は、受けた人がいなければ Nothing」
        it "shows Nothing for a quest nobody took" $ \conn -> do
          addThree conn
          _ <- runSession conn (statement () (R.postQuest "Slay" (Just 1)))
          _ <- runSession conn (statement () (R.postQuest "Find" Nothing))
          let board = Rel8.run (Rel8.select R.questBoard)
          rows <- runSession conn (statement () board)
          -- No ORDER BY, so any order is right.
          -- ORDER BY がないので、順番は問わずに比べる
          rows `shouldMatchList` [("Slay", Just "Lambda"), ("Find", Nothing)]

        -- 「職業ごとに数える」
        it "counts each job" $ \conn -> do
          addThree conn
          let counts = Rel8.run (Rel8.select R.jobCounts)
          rows <- runSession conn (statement () counts)
          rows `shouldMatchList` [("Archer", 1), ("Mage", 2)]
-- ANCHOR_END: spec

-- 🧙 Lambda (3), Python (5), and Merlin (7).
-- Lambda（3）、Python（5）、Merlin（7）を登録する
addThree :: Connection -> IO ()
addThree conn = do
  _ <- runSession conn (statement () (R.addAdventurer "Lambda" "Mage"   3))
  _ <- runSession conn (statement () (R.addAdventurer "Python" "Archer" 5))
  _ <- runSession conn (statement () (R.addAdventurer "Merlin" "Mage"   7))
  pure ()

-- 🧺 Connect, make the tables anew, use, and release.
-- 接続して、表を作り直して、使って、手放す
withFreshGuild :: (Connection -> IO ()) -> T.Text -> IO ()
withFreshGuild action url =
  bracket (connectGuild url) release $ \conn -> do
    runSession conn (sql "SET client_min_messages TO WARNING")
    runSession conn (sql "DROP TABLE IF EXISTS quests, adventurers")
    runSession conn createTables
    action conn

-- 🐳 PostgreSQL 18 in a container (as in the PostgreSQL chapter).
-- コンテナの PostgreSQL 18（PostgreSQL の章と同じ）
postgres :: TestContainer T.Text
postgres = do
  container <- run (containerRequest (fromTag "postgres:18")
    & setEnv [("POSTGRES_PASSWORD", "guild"), ("POSTGRES_DB", "guild")]
    & setExpose ["5432/tcp"]
    & setWaitingFor ready)
  let (host, port) = containerAddress container "5432/tcp"
  pure ("postgresql://postgres:guild@" <> host <> ":"
        <> T.pack (show port) <> "/guild")
  where
    ready     = waitUntilTimeout 60 (setupDone <> portOpen)
    setupDone = waitForLogLine Stdout ("init process complete" `LT.isInfixOf`)
    portOpen  = waitUntilMappedPortReachable "5432/tcp"
-- ANCHOR_END: all
