-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}

-- ⚠️ A bad example: building SQL by gluing strings. Do not do this!
-- 悪い例：文字列をつなげて SQL を作る。まねしないこと！
module Unsafe (findUnsafe) where

import Database.SQLite.Simple
import Data.Text (Text)

import Roster (Adventurer)
-- ANCHOR_END: head

-- ANCHOR: findUnsafe
-- ❌ The name is glued into the SQL as it is.
-- 名前を、そのまま SQL に貼り付けてしまう
findUnsafe :: Connection -> Text -> IO [Adventurer]
findUnsafe conn who =
  query_ conn (Query sql)
  where
    sql = "SELECT id, name, job, level FROM adventurers WHERE name = '"
            <> who <> "'"
-- ANCHOR_END: findUnsafe
-- ANCHOR_END: all
