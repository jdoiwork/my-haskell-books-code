-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}

-- 🪜 Migrations by name: every script that has run is written down.
-- 名前で記録するマイグレーション：動かしたスクリプトを1つずつ書き残す
module Migrate (migrate, appliedScripts, MigrationResult (..)) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration

-- ANCHOR: migrate
-- 🪜 Run the scripts in the directory that have not run yet.
-- ディレクトリーのスクリプトのうち、まだ動かしていないものを動かす
migrate :: Connection -> FilePath -> IO (MigrationResult String)
migrate conn dir =
  runMigrations conn defaultOptions
    [ MigrationInitialization
    , MigrationDirectory dir
    ]
-- ANCHOR_END: migrate

-- ANCHOR: applied
-- 📜 The names of the scripts that have run, in the order they ran.
-- 動かしたスクリプトの名前（動かした順）
appliedScripts :: Connection -> IO [String]
appliedScripts conn = do
  rows <- query_ conn
    "SELECT filename FROM schema_migrations ORDER BY executed_at, filename"
  pure (map fromOnly rows)
-- ANCHOR_END: applied
-- ANCHOR_END: all
