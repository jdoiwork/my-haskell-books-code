-- ANCHOR: all
-- | Choose where the shelf is kept, from a setting such as
--   "sqlite:tsundoku.db".
-- 本棚をどこにしまうかを、"sqlite:tsundoku.db" のような設定で選ぶ
-- ANCHOR: header
module Tsundoku.Shelf.Config
  ( StoreConfig (..)
  , parseStoreConfig
  , withShelf
  ) where

import Data.List (stripPrefix)
import Data.Text qualified as T

import Tsundoku.Shelf
import Tsundoku.Shelf.Memory
import Tsundoku.Shelf.Postgres
import Tsundoku.Shelf.Sqlite
-- ANCHOR_END: header

-- ANCHOR: config
-- ⚙️ Where to keep the shelf.
-- 本棚をしまう場所
data StoreConfig
  = UseMemory              -- memory                    / メモリ
  | UseSqlite FilePath     -- sqlite:tsundoku.db        / SQLite のファイル
  | UsePostgres String     -- postgres:postgresql://... / PostgreSQL
  deriving (Show, Eq)

-- 📖 Read a setting: "memory", "sqlite:FILE", or "postgres:URL".
-- 設定を読む："memory"、"sqlite:ファイル"、"postgres:URL" のどれか
parseStoreConfig :: String -> Either String StoreConfig
parseStoreConfig "memory" = Right UseMemory
parseStoreConfig setting
  | Just path <- stripPrefix "sqlite:"   setting = Right (UseSqlite path)
  | Just url  <- stripPrefix "postgres:" setting = Right (UsePostgres url)
  | otherwise = Left ("unknown store: " <> setting)

-- 🔀 Open the chosen shelf, and lend it to the action.
-- 選んだ本棚を開いて、仕事に貸す
withShelf :: StoreConfig -> (Shelf -> IO a) -> IO a
withShelf UseMemory         action = newMemoryShelf >>= action
withShelf (UseSqlite path)  action = withSqliteShelf path action
withShelf (UsePostgres url) action = withPostgresShelf (T.pack url) action
-- ANCHOR_END: config
-- ANCHOR_END: all
