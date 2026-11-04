-- ⚠️ This file does NOT compile, on purpose (the security chapter).
-- わざとコンパイルできないファイル（セキュリティの章）
{-# LANGUAGE OverloadedStrings #-}
module Concat where

import Data.Text              (Text)
import Database.SQLite.Simple

-- ANCHOR: concat
-- ❌ Glue a Text into the SQL, without saying "Query".
-- Text を SQL に貼り付ける（Query と書かずに）
questsByOwner :: Connection -> Text -> IO [(Text, Text)]
questsByOwner conn who =
  query_ conn ("SELECT title, owner FROM quests WHERE owner = '"
                 <> who <> "'")
-- ANCHOR_END: concat
