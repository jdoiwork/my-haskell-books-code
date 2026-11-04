-- ANCHOR: all
-- ANCHOR: module
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- 🧪 Tests of the roster on a real PostgreSQL in a container.
-- コンテナの中の本物の PostgreSQL で、名簿をテストする
module Main (main) where

-- The roster and its migrations.
-- 名簿とマイグレーション
import PgRoster
import Migrate

-- PostgreSQL, and closing what we opened.
-- PostgreSQL と、開いたものを閉じる道具
import Database.PostgreSQL.Simple
import Control.Exception (bracket)
import Data.ByteString   (ByteString)
import Data.Text         qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy    qualified as LT

-- Containers and hspec.
-- コンテナと hspec
import TestContainers.Hspec
import Test.Hspec
-- ANCHOR_END: module

-- ANCHOR: container
-- 🐳 Start PostgreSQL 18 in a container, and say how to connect to it.
-- コンテナで PostgreSQL 18 を立ち上げて、つなぎ方を返す
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
    -- Wait for the first setup to finish, then for the port to open.
    -- 最初の準備が終わるのを待ってから、ポートが開くのを待つ
    ready     = waitUntilTimeout 60 (setupDone <> portOpen)
    setupDone = waitForLogLine Stdout ("init process complete" `LT.isInfixOf`)
    portOpen  = waitUntilMappedPortReachable "5432/tcp"
-- ANCHOR_END: container

-- ANCHOR: fresh
-- 🧺 Connect, make the tables anew with the migrations, use, and close.
-- 接続して、マイグレーションで表を作り直して、使って、閉じる
withFreshRoster :: (Connection -> IO ()) -> ByteString -> IO ()
withFreshRoster action url =
  bracket (connectPostgreSQL url) close $ \conn -> do
    -- Keep PostgreSQL's NOTICE messages quiet.
    -- PostgreSQL の NOTICE（お知らせ）を出さない
    _ <- execute_ conn "SET client_min_messages TO WARNING"
    _ <- execute_ conn
      "DROP TABLE IF EXISTS adventurers, schema_migrations"
    MigrationSuccess <- migrate conn "migrations"
    action conn
-- ANCHOR_END: fresh

-- ANCHOR: unique
-- 🚫 Is this the error for a broken UNIQUE rule? (SQLSTATE 23505)
-- UNIQUE を破ったエラー？（SQLSTATE 23505）
isUniqueError :: SqlError -> Bool
isUniqueError e = e.sqlState == ("23505" :: ByteString)
-- ANCHOR_END: unique

-- ANCHOR: spec
main :: IO ()
main = hspec $
  aroundAll (withContainers postgres) $
    aroundWith withFreshRoster $ do

      describe "migrations" $
        -- 「スクリプトを、名前で1つずつ記録する」
        it "records each script by name" $ \conn -> do
          names <- appliedScripts conn
          length names `shouldBe` 3

      describe "addAdventurer" $ do
        -- 「付けられた id を返す」
        it "returns the new id" $ \conn -> do
          addAdventurer conn (NewAdventurer "Lambda" "Mage" 3)
            `shouldReturn` 1

        -- 「同じ名前は、2回は登録できない」
        it "rejects the same name twice" $ \conn -> do
          _ <- addAdventurer conn (NewAdventurer "Lambda" "Mage" 3)
          addAdventurer conn (NewAdventurer "Lambda" "Mage" 3)
            `shouldThrow` isUniqueError

      describe "levelUp" $
        -- 「レベルを1上げる」
        it "raises the level by one" $ \conn -> do
          _ <- addAdventurer conn (NewAdventurer "Lambda" "Mage" 3)
          levelUp conn "Lambda" `shouldReturn` 1
          found <- findAdventurer conn "Lambda"
          fmap (.level) found `shouldBe` Just 4
-- ANCHOR_END: spec
-- ANCHOR_END: all
