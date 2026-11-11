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
import Data.Tree (Forest, flatten)
import Database.SQLite.Simple qualified as SQLite
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile)
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
    sqliteOverlapSpec

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
        _ <- shelf.removeWork wid
        tags.tagsOf wid `shouldReturn` []
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
      -- The dragon book last: its entries get new numbers, and the log
      -- follows them.
      -- ドラゴン学の本を最後に：項目の番号が変わり、ログもついていく
      backwards <- newMemoryShelf
      mapM_ (copyCard backwards) (reverse myShelf)
      after <- backwards.listCards
      map (loggedHeadings . snd) after
        `shouldBe` map loggedHeadings (reverse myShelf)
  -- ANCHOR_END: answerMove

-- 📖 The headings of the entries in a card's reading log.
-- カードの読書ログが指す項目の見出し
loggedHeadings :: Card -> [Text]
loggedHeadings card =
  [ h | l <- readLog card, Just h <- [lookup (readEntry l) headings] ]
  where
    headings = case contents (work card) of
      Book   toc -> pairs toc
      Serial toc -> pairs toc
      Series toc -> pairs toc
      Film   _   -> []
      Game       -> []
    pairs toc = [ (entryId e, heading e) | e <- concatMap flatten toc ]
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

-- ANCHOR: sqliteOverlap
-- 🤼 Two programs on one SQLite file. While the other program holds
--    the right to write, our shelf waits (busy_timeout) instead of
--    failing at once. Two programs that open a new file and change it
--    at the same time both get through: the tables are made once, and
--    "read, decide, write" never deadlocks (IMMEDIATE).
-- 1つの SQLite のファイルを、2つのプログラムで。ほかのプログラムが書く権利を
-- 持っているあいだ、こちらの本棚は、すぐ失敗せずに待つ（busy_timeout）。
-- 新しいファイルを同時に開いて同時に変えても、どちらも通る。表を作るのは
-- 1回だけで、「読む → 決める → 書く」が待ち合いにならない（IMMEDIATE）
sqliteOverlapSpec :: Spec
sqliteOverlapSpec = do
  -- 「ほかのプログラムが書いているあいだは、待ってから書く」
  it "waits while another program is writing the file" $
    withTempDb $ \path -> withSqliteShelf path $ \shelf ->
      SQLite.withConnection path $ \other -> do
        SQLite.execute_ other "BEGIN IMMEDIATE"
        answer <- newEmptyMVar
        _      <- forkFinally (shelf.addWork dragonBook) (putMVar answer)
        -- Give the shelf time to come to the lock.
        -- 本棚が鍵のところまで来るのを待つ
        threadDelay 300_000
        SQLite.execute_ other "COMMIT"
        wid <- takeMVar answer >>= either throwIO pure
        fmap (title . work) <$> shelf.getCard wid
          `shouldReturn` Just (title dragonBook)
  -- 「2つのプログラムが、新しいファイルを同時に開いて同時に変えても、
  --   どちらも通る」
  it "lets two programs open and change a new file at the same time" $
    withTempDb $ \path -> do
      let program = withSqliteShelf path $ \shelf ->
            forM_ [1 .. 20 :: Int] $ \_ -> do
              wid <- shelf.addWork dragonBook
              shelf.applyEvent wid Start
      done <- mapM (const newEmptyMVar) [1 .. 2 :: Int]
      forM_ done $ \answer -> forkFinally program (putMVar answer)
      forM_ done $ \answer -> takeMVar answer >>= either throwIO pure
      cards <- withSqliteShelf path (\shelf -> shelf.listCards)
      map (reading . snd) cards `shouldBe` replicate 40 Reading

-- 🗂️ A new, empty SQLite file in the temporary folder, removed after.
-- 一時フォルダーに、新しい空の SQLite のファイル。終わったら消す
withTempDb :: (FilePath -> IO a) -> IO a
withTempDb action = do
  tmp <- getTemporaryDirectory
  bracket (openTempFile tmp "shelf.db") (removeFile . fst) $
    \(path, handle) -> hClose handle >> action path
-- ANCHOR_END: sqliteOverlap

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
