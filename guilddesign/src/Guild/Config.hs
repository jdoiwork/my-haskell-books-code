-- ANCHOR: all
-- ⚙️ Choose the store from a setting, such as "sqlite:guild.db".
-- 設定（"sqlite:guild.db" など）から、置き場を選ぶ
-- ANCHOR: imports
module Guild.Config
  ( StoreConfig (..)
  , parseStoreConfig
  , withStore
  ) where

import Data.ByteString.Char8 qualified as B
import Data.List             (stripPrefix)

import Guild.Store
import Guild.Store.Memory
import Guild.Store.Sqlite
import Guild.Store.Postgres
-- ANCHOR_END: imports

-- ANCHOR: config
-- ⚙️ Which store to use.
-- どの置き場を使うか
data StoreConfig
  = UseMemory              -- memory               / メモリー
  | UseSqlite FilePath     -- sqlite:guild.db      / SQLite のファイル
  | UsePostgres String     -- postgres:postgresql://... / PostgreSQL
  deriving (Show, Eq)

-- 📖 Read a setting. "memory", "sqlite:FILE", or "postgres:URL".
-- 設定を読む。"memory"、"sqlite:ファイル"、"postgres:URL" のどれか
parseStoreConfig :: String -> Either String StoreConfig
parseStoreConfig "memory" = Right UseMemory
parseStoreConfig setting
  | Just path <- stripPrefix "sqlite:"   setting = Right (UseSqlite path)
  | Just url  <- stripPrefix "postgres:" setting = Right (UsePostgres url)
  | otherwise = Left ("unknown store: " <> setting)
-- ANCHOR_END: config

-- ANCHOR: withStore
-- 🔀 Open the chosen store, and lend it to the action.
-- 選んだ置き場を開いて、仕事に貸す
withStore :: StoreConfig -> (RosterStore -> IO a) -> IO a
withStore UseMemory         action = newMemoryStore >>= action
withStore (UseSqlite path)  action = withSqliteStore path action
withStore (UsePostgres url) action = withPostgresStore (B.pack url) action
-- ANCHOR_END: withStore
-- ANCHOR_END: all
