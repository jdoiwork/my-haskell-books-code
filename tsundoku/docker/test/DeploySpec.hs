-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
-- 🧪 Handing the shelf out: the store setting with a password from a
--    file, opening again while the database starts, and the backup.
-- 本棚を配るためのテスト：ファイルからのパスワードつきのしまう場所、
-- データベースが起きるまでの開き直し、バックアップ
module DeploySpec (deploySpec) where

import Control.Exception (ErrorCall (..), throwIO, try)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Time (diffTimeToPicoseconds, getCurrentTime, utctDayTime)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory,
                         removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

import Tsundoku.Deploy
import Tsundoku.Examples (dragonBook)
import Tsundoku.Shelf
import Tsundoku.Shelf.Sqlite (backupSqlite, withSqliteShelf)

import ShelfAnswers.Rotate
-- ANCHOR_END: imports

-- ANCHOR: deploySpec
deploySpec :: Spec
deploySpec = do
  describe "the store setting" $ do
    -- 「なければ SQLite のファイル」
    it "is SQLite's file when unset" $
      storeSetting Nothing Nothing `shouldBe` "sqlite:tsundoku.db"
    -- 「PostgreSQL には、ファイルのパスワードを足す（改行は落とす）」
    it "adds the password from a file to PostgreSQL's" $
      storeSetting (Just "postgres:host=db user=shelf") (Just "s3cret\n")
        `shouldBe` "postgres:host=db user=shelf password='s3cret'"
    -- 「パスワードの中の ' と \ はエスケープする」
    it "escapes quotes and backslashes in the password" $
      storeSetting (Just "postgres:host=db") (Just "it's\\x")
        `shouldBe` "postgres:host=db password='it\\'s\\\\x'"
    -- 「SQLite には、パスワードを足さない」
    it "leaves SQLite as it is" $
      storeSetting (Just "sqlite:/data/t.db") (Just "s3cret")
        `shouldBe` "sqlite:/data/t.db"

  describe "opening again" $ do
    -- 「開けないうちは、やり直す」
    it "tries again while opening fails" $ do
      opens <- newIORef (0 :: Int)
      let open work = do
            modifyIORef' opens (+ 1)
            n <- readIORef opens
            if n < 3 then throwIO (ErrorCall "not yet") else work ()
      retryOpen 5 0 open (\() -> pure "opened") `shouldReturn` ("opened" :: String)
      readIORef opens `shouldReturn` 3
    -- 「仕事が始まってからの失敗は、やり直さない」
    it "does not retry once the work has begun" $ do
      opens <- newIORef (0 :: Int)
      let open work = modifyIORef' opens (+ 1) >> work ()
      result <- try (retryOpen 5 0 open (\() -> throwIO (ErrorCall "work")))
      (result :: Either ErrorCall ()) `shouldBe` Left (ErrorCall "work")
      readIORef opens `shouldReturn` 1
    -- 「何度でも開けなければ、最後の失敗を投げる」
    it "gives up after the last try" $ do
      let open :: (() -> IO ()) -> IO ()
          open _ = throwIO (ErrorCall "never")
      retryOpen 3 0 open (\() -> pure ()) `shouldThrow` (== ErrorCall "never")

  describe "the backup" $
    -- 「VACUUM INTO で写したファイルは、本棚として開ける」
    it "copies the shelf into a file that opens as a shelf" $ do
      tmp   <- getTemporaryDirectory
      stamp <- show . diffTimeToPicoseconds . utctDayTime <$> getCurrentTime
      let dir  = tmp </> ("tsundoku-backup-" <> stamp)
          from = dir </> "from.db"
          to   = dir </> "to.db"
      createDirectoryIfMissing True dir
      withSqliteShelf from $ \shelf -> () <$ shelf.works.addWork dragonBook
      backupSqlite from to
      withSqliteShelf to $ \shelf ->
        length <$> shelf.works.listCards `shouldReturn` 1
      removeDirectoryRecursive dir

  describe "the answers" $
    -- 「新しいものを残して、古いバックアップを消す」
    it "removes all but the newest backups" $
      toRemove 2 [ "tsundoku.db", "backup-2026-11-07.db", "backup-2026-11-09.db"
                 , "backup-2026-11-08.db", "notes.txt" ]
        `shouldBe` ["backup-2026-11-07.db"]
-- ANCHOR_END: deploySpec
-- ANCHOR_END: all
