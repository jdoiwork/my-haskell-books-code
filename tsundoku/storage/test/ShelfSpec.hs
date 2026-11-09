-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
-- 🧪 The contract, on all three shelves, and the tree as lines.
-- 契約のテストを3つの本棚で。それと、木を行にするテスト
module ShelfSpec (shelfSpec) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Tree (Forest)
import Database.SQLite.Simple qualified as SQLite
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import TestContainers.Hspec
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Session qualified as Session
import Pqi.Ffi qualified as Pqi

import ShelfAnswers.Move
import ShelfAnswers.Orphans
import ShelfAnswers.Tags
import ShelfContract
import Tsundoku.Card (Card (..), ratingsOf)
import Tsundoku.Examples
import Tsundoku.Outline
import Tsundoku.Shelf
import Tsundoku.Shelf.Memory
import Tsundoku.Shelf.Postgres
import Tsundoku.Shelf.Sqlite
import Tsundoku.Types
-- ANCHOR_END: imports

-- ANCHOR: spec
shelfSpec :: Spec
shelfSpec = do
  -- 「木を深さつきの行にして、また木に戻す」
  describe "a tree as lines" $ do
    -- 「ドラゴン学の目次は、深さ 0・1・2 の行になる」
    it "writes the dragon book down with depths" $
      map fst (outline dragonToc) `shouldBe` [0, 1, 2, 2, 1, 0, 1, 1]
    -- 「行から木に戻すと、もとの木」
    prop "grows back the same forest" $ \forest ->
      fromOutline (outline forest) == (forest :: Forest Int)

  describe "shelf in memory" $
    around withMemory shelfContract

  describe "shelf in SQLite" $ do
    around (withSqliteShelf ":memory:") (shelfContract >> tornContract)
    -- 「作品を消すと、その感想も消える（外部キーが効いている）」
    it "removes the notes with the work" $
      SQLite.withConnection ":memory:" $ \conn -> do
        shelf <- sqliteShelf conn
        wid   <- shelf.addWork dragonBook
        _     <- shelf.addNote wid (Note OnWork "!" (onDay 2026 11 9) False)
        _     <- shelf.removeWork wid
        left  <- SQLite.query_ conn "SELECT count(*) FROM notes"
        left `shouldBe` [SQLite.Only (0 :: Int)]

  describe "shelf in PostgreSQL" $ do
    docker <- runIO dockerIsRunning
    if docker
      then aroundAll (withContainers postgres) $
             aroundWith withFreshPostgres (shelfContract >> tornContract)
      else around skip (shelfContract >> tornContract)

  -- 「トレーニングの解答」
  describe "answers (shelf)" shelfAnswers

-- 🧠 A new, empty shelf in memory.
-- メモリに、空っぽの本棚を新しく
withMemory :: (Shelf -> IO ()) -> IO ()
withMemory action = newMemoryShelf >>= action
-- ANCHOR_END: spec

-- ANCHOR: answers
-- 🏋️ The answers of the training.
-- トレーニングの解答
shelfAnswers :: Spec
shelfAnswers = do
  -- 「外部キーを切って消すと、感想が置いていかれる」
  it "leaves notes behind without foreign keys" $
    SQLite.withConnection ":memory:" $ \conn -> do
      shelf <- sqliteShelf conn
      wid   <- shelf.addWork dragonBook
      _     <- shelf.addNote wid (Note OnWork "!" (onDay 2026 11 9) False)
      SQLite.execute_ conn "PRAGMA foreign_keys = OFF"
      _     <- shelf.removeWork wid
      orphanNotes conn `shouldReturn` 1
  -- 「タグは、メモリでも SQLite でも同じに付く」
  it "tags works in memory and in SQLite alike" $
    SQLite.withConnection ":memory:" $ \conn -> do
      inMemory <- newMemoryShelf
      inSqlite <- sqliteShelf conn
      taggings <- sequence [ memoryTagging inMemory
                           , sqliteTagging inSqlite conn ]
      forM_ (zip [inMemory, inSqlite] taggings) $ \(shelf, tags) -> do
        wid <- shelf.addWork dragonBook
        mapM_ (tags.tagWork wid) [" 魔法 ", "ドラゴン", "魔法"]
        tags.tagsOf wid `shouldReturn` ["ドラゴン", "魔法"]
        tags.tagWork (WorkId 9) "?"
          `shouldReturn` Left (NoSuchWork (WorkId 9))
  -- ANCHOR: answerTorn
  -- 「-1 ページの本を、メモリはしまい、SQLite は断る」
  it "finds a work memory keeps and SQLite refuses" $
    SQLite.withConnection ":memory:" $ \conn -> do
      inMemory <- newMemoryShelf
      inSqlite <- sqliteShelf conn
      _        <- inMemory.addWork tornWork
      refused  <- try (inSqlite.addWork tornWork)
      map fst <$> inMemory.listCards `shouldReturn` [WorkId 1]
      either (const True) (const False)
        (refused :: Either SomeException WorkId) `shouldBe` True
  -- ANCHOR_END: answerTorn
  -- ANCHOR: answerMove
  -- 「メモリの本棚を、SQLite へまるごと引っ越す」
  it "moves a whole shelf from memory to SQLite" $
    SQLite.withConnection ":memory:" $ \conn -> do
      inMemory <- newMemoryShelf
      mapM_ (copyCard inMemory) myShelf
      inSqlite <- sqliteShelf conn
      moved    <- moveShelf inMemory inSqlite
      map snd moved `shouldBe` map WorkId [1 .. 6]
      before <- inMemory.listCards
      inSqlite.listCards `shouldReturn` before
      map (title . work . snd) before `shouldBe` map (title . work) myShelf
      map (ratingsOf . snd) before `shouldBe` map ratingsOf myShelf
      map (reading . snd) before `shouldBe` map reading myShelf
      map (length . notes . snd) before
        `shouldBe` map (length . notes) myShelf
  -- ANCHOR_END: answerMove
-- ANCHOR_END: answers

-- ANCHOR: docker
-- 🐳 Is Docker there and running? Without it, the PostgreSQL tests are
--    marked pending: the same number of tests, run or not.
-- Docker があって、動いている？ なければ、PostgreSQL のテストは
-- 保留（pending）にする。動かしても動かさなくても、テストの数は同じ
dockerIsRunning :: IO Bool
dockerIsRunning = do
  result <- try (readProcessWithExitCode "docker" ["info"] "")
  pure $ case result :: Either SomeException (ExitCode, String, String) of
    Right (ExitSuccess, _, _) -> True
    _                         -> False

skip :: (Shelf -> IO ()) -> IO ()
skip _ = pendingWith "Docker is not running / Docker が動いていない"
-- ANCHOR_END: docker

-- ANCHOR: postgres
-- 🐘 Empty tables each time: drop everything, and withPostgresShelf
--    builds the tables again.
-- 毎回、空っぽの表：全部消しておけば、withPostgresShelf が作り直す
withFreshPostgres :: (Shelf -> IO ()) -> Text -> IO ()
withFreshPostgres action url = do
  acquired <- Connection.acquire Pqi.adapter (Settings.connectionString url)
  case acquired of
    Left problem -> fail (show problem)
    Right conn   -> do
      void (Connection.use conn
        (Session.script "DROP SCHEMA public CASCADE; CREATE SCHEMA public"))
      Connection.release conn
  withPostgresShelf url action

-- 🐳 PostgreSQL 18 in a container (as in the PostgreSQL chapter).
-- コンテナの PostgreSQL 18（PostgreSQL の章と同じ）
postgres :: TestContainer Text
postgres = do
  container <- run (containerRequest (fromTag "postgres:18")
    & setEnv [("POSTGRES_PASSWORD", "shelf"), ("POSTGRES_DB", "shelf")]
    & setExpose ["5432/tcp"]
    & setWaitingFor ready)
  let (host, port) = containerAddress container "5432/tcp"
  pure (T.unwords
    [ "host=" <> host
    , "port=" <> T.pack (show port)
    , "user=postgres password=shelf dbname=shelf"
    ])
  where
    ready     = waitUntilTimeout 60 (setupDone <> portOpen)
    setupDone = waitForLogLine Stdout ("init process complete" `LT.isInfixOf`)
    portOpen  = waitUntilMappedPortReachable "5432/tcp"
-- ANCHOR_END: postgres
-- ANCHOR_END: all
