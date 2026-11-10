-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
-- 🧪 The contract, on all three shelves, and the tree as lines.
-- 契約のテストを3つの本棚で。それと、木を行にするテスト
module ShelfSpec (shelfSpec) where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar,
                           threadDelay)
import Control.Exception (SomeException, bracket, throwIO, try)
import Control.Monad (forM_, unless, void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Tree (Forest)
import Database.SQLite.Simple qualified as SQLite
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import TestContainers.Hspec hiding (Paused)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Decoders qualified as D
import Hasql.Session qualified as Session
import Hasql.Statement (preparable)
import Pqi.Ffi qualified as Pqi

import ShelfAnswers.Move
import ShelfAnswers.NotesOnly
import ShelfAnswers.Orphans
import ShelfAnswers.Spoilers
import ShelfAnswers.Tags
import ShelfContract
import Tsundoku.Card (Card (..), ratingsOf)
import Tsundoku.Examples
import Tsundoku.Outline
import Tsundoku.Search (Hit (..), Place (..))
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
    -- ANCHOR: earlierFile
    -- 「前の章までのファイルにあったものも、探せる」
    it "finds what a file from the earlier chapters had" $
      SQLite.withConnection ":memory:" $ \conn -> do
        -- a file made by the earlier chapters: steps 1 and 2, one work
        -- 前の章までが作ったファイル：手順1と2、作品が1つ
        forM_ (take 2 migrations) (mapM_ (SQLite.execute_ conn))
        SQLite.execute_ conn "PRAGMA user_version = 2"
        SQLite.execute_ conn
          "INSERT INTO works (title, creator, kind, reading) \
          \VALUES ('古い本棚の本', 'だれか', 'game', 'Piled')"
        shelf <- sqliteShelf conn
        fmap (map place) <$> shelf.searchText "古い本棚"
          `shouldReturn` Right [InTitle]
    -- ANCHOR_END: earlierFile

  describe "shelf in PostgreSQL" $ do
    docker <- runIO dockerIsRunning
    if docker
      then aroundAll (withContainers postgres) $ do
             aroundWith withFreshPostgres (shelfContract >> tornContract)
             overlapSpec
      else do
        around skip (shelfContract >> tornContract)
        around skip overlapSpec

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
  -- ANCHOR: answerSpoilers
  -- 「感想だけを探す」
  it "searches the notes only" $ do
    shelf <- newMemoryShelf
    _     <- fillForSearch shelf
    fmap (map place) <$> searchNotes shelf "火を吐"
      `shouldReturn` Right [InNote (OnEntry (EntryId 4)) True]
  -- 「読み終えるまでは、ネタバレの感想を外す」
  it "leaves out spoilers until the work is read" $ do
    shelf <- newMemoryShelf
    _     <- fillForSearch shelf
    _     <- shelf.applyEvent (WorkId 1) Start
    Right hits <- shelf.searchText "火を吐"
    any (isNote . place) hits `shouldBe` True
    map place <$> withoutSpoilers shelf hits
      `shouldReturn` filter (not . isNote) (map place hits)
    _     <- shelf.applyEvent (WorkId 1) Finish
    map place <$> withoutSpoilers shelf hits
      `shouldReturn` map place hits
  -- ANCHOR_END: answerSpoilers
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

skip :: (a -> IO ()) -> IO ()
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

-- 🐳 PostgreSQL 18 with PGroonga in a container: PGroonga's own image
--    (the PostgreSQL chapter used postgres:18, which has no PGroonga).
-- コンテナの PostgreSQL 18 と PGroonga：PGroonga の公式のイメージ
-- （PostgreSQL の章の postgres:18 には、PGroonga が入っていない）
postgres :: TestContainer Text
postgres = do
  container <- run (containerRequest (fromTag pgroonga)
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
    pgroonga  = "groonga/pgroonga:4.1.0-debian-18"
    ready     = waitUntilTimeout 60 (setupDone <> portOpen)
    setupDone = waitForLogLine Stdout ("init process complete" `LT.isInfixOf`)
    portOpen  = waitUntilMappedPortReachable "5432/tcp"
-- ANCHOR_END: postgres

-- ANCHOR: overlap
-- 🤼 Another program pauses the book while our shelf finishes it. The
--    test holds the row with a second connection, waits until the shelf
--    is stuck on it, and only then commits: no sleep sets the order.
-- ほかのプログラムが本を中断にするあいだに、こちらの本棚が読み終える。
-- テストは2本目の接続で行を押さえ、本棚がそこで待つのを見てから確定する。
-- 順番を決めるのは、sleep ではない
overlapSpec :: SpecWith Text
overlapSpec =
  -- 「ほかのプログラムが中断にした本を、古いカードから読了にしない」
  it "does not finish a book another program has just paused" $ \url ->
    withFreshPostgres (finishWhilePaused url) url

finishWhilePaused :: Text -> Shelf -> IO ()
finishWhilePaused url shelf = do
  wid <- shelf.addWork dragonBook
  _   <- shelf.applyEvent wid Start
  withConnection url $ \other -> do
    -- The other program pauses the book, but does not commit yet.
    -- ほかのプログラムが本を中断にする。でも、まだ確定しない
    script other ("BEGIN; UPDATE works SET reading = 'Paused' WHERE id = "
                    <> T.pack (show (workNumber wid)))
    -- Our shelf reads Reading, decides Finished, and waits for the row.
    -- こちらの本棚は「読書中」を読み、読了と決めて、行を待つ
    answer <- newEmptyMVar
    _      <- forkFinally (shelf.applyEvent wid Finish) (putMVar answer)
    waitForLock other
    script other "COMMIT"
    -- PostgreSQL says the row changed after we read it, and the job runs
    -- again on the new card: a paused book cannot be finished.
    -- 読んだあとに行が変わったと PostgreSQL が言い、仕事は新しいカードで
    -- やり直す。中断の本は、読了にできない
    finished <- takeMVar answer >>= either throwIO pure
    finished `shouldBe` Left (NotAllowed Finish Paused)
    fmap reading <$> shelf.getCard wid `shouldReturn` Just Paused
  where
    workNumber (WorkId w) = w

-- 🔌 A second connection of our own, for the other program.
-- ほかのプログラムの役をする、2本目の接続
withConnection :: Text -> (Connection.Connection -> IO a) -> IO a
withConnection url = bracket open Connection.release
  where
    open = Connection.acquire Pqi.adapter (Settings.connectionString url)
             >>= either (fail . show) pure

-- 📜 Send some SQL on the connection, and fail if PostgreSQL says no.
-- 接続で SQL を送る。PostgreSQL が断ったら、失敗にする
script :: Connection.Connection -> Text -> IO ()
script conn sql =
  Connection.use conn (Session.script sql) >>= either (fail . show) pure

-- ⏳ Wait until some connection waits for a lock (pg_locks shows it as
--    not granted), up to 10 seconds. Asking again only waits; the lock
--    sets the order.
-- だれかの接続が鍵を待つまで、10秒まで待つ（pg_locks で、まだ渡されて
-- いない鍵）。聞き直すのは待つためだけで、順番を決めるのは鍵
waitForLock :: Connection.Connection -> IO ()
waitForLock conn = do
  seen <- timeout 10_000_000 askAgain
  unless (seen == Just ()) (expectationFailure "nobody waited for a lock")
  where
    askAgain = do
      found <- Connection.use conn (Session.statement () waiting)
      case found of
        Right n | n > 0 -> pure ()
        Right _         -> threadDelay 10_000 >> askAgain
        Left problem    -> fail (show problem)
    waiting = preparable
      "SELECT count(*) FROM pg_locks WHERE NOT granted"
      mempty
      (D.singleRow (D.column (D.nonNullable D.int8)))
-- ANCHOR_END: overlap
-- ANCHOR_END: all
