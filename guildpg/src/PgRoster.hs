-- ANCHOR: all
-- ANCHOR: pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- ANCHOR_END: pragma

-- ANCHOR: module
-- 🐘 The guild's roster, on PostgreSQL.
-- ギルドの名簿（PostgreSQL 版）
module PgRoster
  ( Adventurer (..)
  , NewAdventurer (..)
  , addAdventurer
  , listAdventurers
  , findAdventurer
  , levelUp
  , removeAdventurer
  ) where

-- PostgreSQL: connections, queries, and rows.
-- PostgreSQL：接続、問い合わせ、行
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToRow   (ToRow (..))

-- Text, numbers of rows, and the first element of a list.
-- テキスト、行の数、リストの最初の要素
import Data.Text  (Text)
import Data.Int   (Int64)
import Data.Maybe (listToMaybe)
-- ANCHOR_END: module

-- ANCHOR: types
-- 🧙 An adventurer, as read from the table.
-- 表から読んだ冒険者
data Adventurer = Adventurer
  { adventurerId :: Int
  , name         :: Text
  , job          :: Text
  , level        :: Int
  }
  deriving (Show, Eq)

-- ✍️ A new adventurer, before the database gives it an id.
-- 新しい冒険者（データベースが id を付ける前）
data NewAdventurer = NewAdventurer
  { newName  :: Text
  , newJob   :: Text
  , newLevel :: Int
  }
  deriving (Show, Eq)

-- 📥📤 Rows, just as with sqlite-simple.
-- 行の読み書き（sqlite-simple と同じ）
instance FromRow Adventurer where
  fromRow = Adventurer <$> field <*> field <*> field <*> field

instance ToRow NewAdventurer where
  toRow a = toRow (a.newName, a.newJob, a.newLevel)
-- ANCHOR_END: types

-- ANCHOR: create
-- ➕ Create: add an adventurer, and get the id it was given.
-- 登録：冒険者を足して、付けられた id を受け取る
addAdventurer :: Connection -> NewAdventurer -> IO Int
addAdventurer conn new = do
  rows <- query conn
    "INSERT INTO adventurers (name, job, level) VALUES (?, ?, ?) \
    \RETURNING id"
    new
  case rows of
    [Only newId] -> pure newId
    _            -> fail "INSERT ... RETURNING gave no id"
-- ANCHOR_END: create

-- ANCHOR: read
-- 📋 Read: every adventurer, in the order they joined.
-- 一覧：全員を、登録した順に
listAdventurers :: Connection -> IO [Adventurer]
listAdventurers conn =
  query_ conn "SELECT id, name, job, level FROM adventurers ORDER BY id"

-- 🔍 Read: the adventurer with this name, if any.
-- 検索：この名前の冒険者（いれば）
findAdventurer :: Connection -> Text -> IO (Maybe Adventurer)
findAdventurer conn who = do
  rows <- query conn
    "SELECT id, name, job, level FROM adventurers WHERE name = ?"
    (Only who)
  pure (listToMaybe rows)
-- ANCHOR_END: read

-- ANCHOR: update
-- ⬆️ Update: one level up. execute returns how many rows changed.
-- 更新：レベルを1上げる。execute が、変わった行の数を返す
levelUp :: Connection -> Text -> IO Int64
levelUp conn who =
  execute conn
    "UPDATE adventurers SET level = level + 1 WHERE name = ?"
    (Only who)

-- 🚪 Delete: remove from the roster.
-- 削除：名簿から外す
removeAdventurer :: Connection -> Text -> IO Int64
removeAdventurer conn who =
  execute conn "DELETE FROM adventurers WHERE name = ?" (Only who)
-- ANCHOR_END: update
-- ANCHOR_END: all
