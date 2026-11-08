-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- 🏰 The guild's work. It uses a store, but never asks which one.
-- ギルドの仕事。置き場を使うが、どの置き場かは気にしない
module Guild.Service (enlist, rosterReport) where

import Data.Text (Text)
import Data.Text qualified as T

import Guild.Roster
import Guild.Store
-- ANCHOR_END: imports

-- ANCHOR: enlist
-- 📝 Add a new adventurer, and say how it went.
-- 新しい冒険者を登録して、結果を伝える
enlist :: RosterStore -> NewAdventurer -> IO Text
enlist store new = do
  result <- store.addAdventurer new
  case result of
    -- 「Lambda さんを、3番で登録しました」
    Right newId -> pure ("Welcome, " <> new.newName
                         <> "! (no. " <> T.pack (show newId) <> ")")
    -- 「Lambda という名前は、もう使われています」
    Left (NameTaken who) -> pure ("The name " <> who <> " is taken.")

-- 📋 The whole roster, one line each.
-- 名簿の全体（1人1行）
rosterReport :: RosterStore -> IO [Text]
rosterReport store = do
  everyone <- store.listAdventurers
  pure (map formatAdventurer everyone)
-- ANCHOR_END: enlist
-- ANCHOR_END: all
