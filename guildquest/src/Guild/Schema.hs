-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}

-- ANCHOR: module
-- 🏗️ The shape of the guild's database, and how it grows.
-- ギルドのデータベースの形と、その育て方
module Guild.Schema
  ( migrations
  , schemaVersion
  , migrate
  , migrateWith
  ) where

-- SQLite.
-- SQLite
import Database.SQLite.Simple

-- Repeating an action for each step, and Text.
-- 1段ずつくり返す道具と、テキスト
import Control.Monad (forM_)
import Data.Text     qualified as T
-- ANCHOR_END: module

-- ANCHOR: migrations
-- 📜 The steps that grow the database, from version 1 on.
--    Never change a step that is already used. Add a new one instead.
-- データベースを育てる段。版1から順に並べる
-- 一度使った段は書き換えない。変えたいときは、新しい段を足す
migrations :: [Query]
migrations =
  [ -- 1: the roster of adventurers / 冒険者の名簿
    """
    CREATE TABLE adventurers
      ( id    INTEGER PRIMARY KEY
      , name  TEXT    NOT NULL UNIQUE
      , job   TEXT    NOT NULL
      , level INTEGER NOT NULL
      )
    """
  , -- 2: the quests / 依頼
    """
    CREATE TABLE quests
      ( id       INTEGER PRIMARY KEY
      , title    TEXT    NOT NULL
      , reward   INTEGER NOT NULL
      , taken_by INTEGER REFERENCES adventurers (id)
      , done     INTEGER NOT NULL DEFAULT 0
      )
    """
  , -- 3: the gold of each adventurer / 冒険者の所持金
    "ALTER TABLE adventurers ADD COLUMN gold INTEGER NOT NULL DEFAULT 0"
  ]
-- ANCHOR_END: migrations

-- ANCHOR: version
-- 🔢 The version of the database (0 for a new, empty one).
-- データベースの版（新しい空のデータベースなら 0）
schemaVersion :: Connection -> IO Int
schemaVersion conn = do
  rows <- query_ conn "PRAGMA user_version"
  case rows of
    [Only v] -> pure v
    _        -> pure 0
-- ANCHOR_END: version

-- ANCHOR: migrate
-- ⬆️ Run the steps the database has not had yet, one by one.
--    Each step and its new version go in one transaction.
--    Returns the version after that.
-- まだ通っていない段を、1つずつ動かす
-- 段と、新しい版の書き込みを、1つのトランザクションにする
-- 終わったあとの版を返す
migrateWith :: [Query] -> Connection -> IO Int
migrateWith steps conn = do
  current <- schemaVersion conn
  let pending = drop current (zip [1 :: Int ..] steps)
  forM_ pending $ \(version, step) ->
    withTransaction conn $ do
      execute_ conn step
      execute_ conn (setVersion version)
  schemaVersion conn
  where
    -- PRAGMA cannot take ?, so the number goes into the SQL itself.
    -- It is our own number, never a value from outside.
    -- PRAGMA には ? が使えないので、数を SQL に直接入れる
    -- 外から来た値ではなく、自分の数なので安全
    setVersion v = Query ("PRAGMA user_version = " <> T.pack (show v))

-- ⬆️ Grow the guild's database to the newest version.
-- ギルドのデータベースを、いちばん新しい版まで育てる
migrate :: Connection -> IO Int
migrate = migrateWith migrations
-- ANCHOR_END: migrate
-- ANCHOR_END: all
