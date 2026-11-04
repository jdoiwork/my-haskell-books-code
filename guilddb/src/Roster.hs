-- ANCHOR: all
-- ANCHOR: pragma
-- Let us write SQL and Text as "..." literals, and SQL in many lines.
-- SQL や Text を "..." で書けるようにする
-- 何行にもなる SQL も書けるようにする
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}
-- Let us write a.name to read a field.
-- a.name のようにフィールドを読めるようにする
{-# LANGUAGE OverloadedRecordDot #-}
-- ANCHOR_END: pragma

-- ANCHOR: module
-- 📒 The guild's roster, kept in a SQLite database.
-- ギルドの名簿。SQLite のデータベースに保存する
module Roster
  ( Adventurer (..)
  , NewAdventurer (..)
  , createTable
  , addAdventurer
  , listAdventurers
  , findAdventurer
  , levelUp
  , removeAdventurer
  , formatAdventurer
  ) where

-- SQLite: connections, queries, and rows.
-- SQLite：接続、問い合わせ、行
import Database.SQLite.Simple

-- Text, and the first element of a list (if any).
-- テキストと、リストの最初の要素（あれば）
import Data.Text  (Text)
import Data.Text  qualified as T
import Data.Maybe (listToMaybe)
-- ANCHOR_END: module

-- ANCHOR: createTable
-- 🧱 Make the table of adventurers, if it is not there yet.
-- 冒険者の表を作る（まだなければ）
createTable :: Connection -> IO ()
createTable conn = execute_ conn """
  CREATE TABLE IF NOT EXISTS adventurers
    ( id    INTEGER PRIMARY KEY
    , name  TEXT    NOT NULL UNIQUE
    , job   TEXT    NOT NULL
    , level INTEGER NOT NULL
    )
  """
-- ANCHOR_END: createTable

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
-- ANCHOR_END: types

-- ANCHOR: rows
-- 📥 One row of the table, as an Adventurer: id, name, job, level.
-- 表の1行を Adventurer にする：id、name、job、level の順
instance FromRow Adventurer where
  fromRow = Adventurer <$> field <*> field <*> field <*> field

-- 📤 A NewAdventurer, as the values for the three ? of INSERT.
-- NewAdventurer を、INSERT の3つの ? に入れる値にする
instance ToRow NewAdventurer where
  toRow a = toRow (a.newName, a.newJob, a.newLevel)
-- ANCHOR_END: rows

-- ANCHOR: create
-- ➕ Create: add an adventurer to the roster.
-- 登録：名簿に冒険者を足す
addAdventurer :: Connection -> NewAdventurer -> IO ()
addAdventurer conn new =
  execute conn
    "INSERT INTO adventurers (name, job, level) VALUES (?, ?, ?)"
    new
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
-- ⬆️ Update: one level up. Returns how many rows changed (0 or 1).
-- 更新：レベルを1上げる。変わった行の数（0 か 1）を返す
levelUp :: Connection -> Text -> IO Int
levelUp conn who = do
  execute conn
    "UPDATE adventurers SET level = level + 1 WHERE name = ?"
    (Only who)
  changes conn
-- ANCHOR_END: update

-- ANCHOR: delete
-- 🚪 Delete: remove from the roster. Returns how many rows changed.
-- 削除：名簿から外す。変わった行の数を返す
removeAdventurer :: Connection -> Text -> IO Int
removeAdventurer conn who = do
  execute conn "DELETE FROM adventurers WHERE name = ?" (Only who)
  changes conn
-- ANCHOR_END: delete

-- ANCHOR: format
-- 🖨️ One line for the roster: "Lambda      Mage      Lv 3"
-- 名簿の1行
formatAdventurer :: Adventurer -> Text
formatAdventurer a =
  T.justifyLeft 12 ' ' a.name
    <> T.justifyLeft 10 ' ' a.job
    <> "Lv " <> T.pack (show a.level)
-- ANCHOR_END: format
-- ANCHOR_END: all
