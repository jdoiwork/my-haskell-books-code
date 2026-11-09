-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- | Training, step 2: tags on works, as a small window of its own that
--   stands on the shelf's window.
-- トレーニングのステップ2：作品のタグ。本棚の窓口の上に立つ、小さな窓口
module ShelfAnswers.Tags
  ( Tagging (..)
  , memoryTagging
  , sqliteTagging
  ) where

import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple

import Tsundoku.Shelf
import Tsundoku.Types
-- ANCHOR_END: head

-- ANCHOR: tagging
-- 🏷️ What you can do with tags.
-- タグでできること
data Tagging = Tagging
  { tagWork :: WorkId -> Text -> IO (Checked ())
  , tagsOf  :: WorkId -> IO [Text]
  }

-- 🧠 Tags in memory. The shelf says whether the work is there.
-- メモリのタグ。作品があるかどうかは、本棚に聞く
memoryTagging :: Shelf -> IO Tagging
memoryTagging shelf = do
  ref <- newIORef Map.empty
  pure Tagging
    { tagWork = \wid name -> onWork shelf wid $
        atomicModifyIORef' ref (\m ->
          (Map.insertWith Set.union wid (Set.singleton (clean name)) m, ()))
    , tagsOf  = \wid ->
        maybe [] Set.toAscList . Map.lookup wid <$> readIORef ref
    }

-- 🪶 Tags in SQLite: one more table. In the real code, this would be
--    step 2 at the end of `migrations`.
-- SQLite のタグ：表を1つ足す。本物のコードなら、migrations の最後の
-- 手順2にする
sqliteTagging :: Shelf -> Connection -> IO Tagging
sqliteTagging shelf conn = do
  execute_ conn """
    CREATE TABLE IF NOT EXISTS tags
      ( work_id INTEGER NOT NULL REFERENCES works ON DELETE CASCADE
      , name    TEXT    NOT NULL
      , PRIMARY KEY (work_id, name)
      )
    """
  pure Tagging
    { tagWork = \wid@(WorkId w) name -> onWork shelf wid $
        execute conn
          "INSERT OR IGNORE INTO tags (work_id, name) VALUES (?, ?)"
          (w, clean name)
    , tagsOf  = \(WorkId w) -> map fromOnly <$> query conn
        "SELECT name FROM tags WHERE work_id = ? ORDER BY name" (Only w)
    }

-- 🎯 Do it only if the work is on the shelf.
-- 本棚に作品があるときだけ、する
onWork :: Shelf -> WorkId -> IO () -> IO (Checked ())
onWork shelf wid action = do
  found <- shelf.getCard wid
  case found of
    Nothing -> pure (Left (NoSuchWork wid))
    Just _  -> Right <$> action

-- ✂️ " SF " and "SF" are the same tag.
-- " SF " と "SF" は、同じタグ
clean :: Text -> Text
clean = T.strip
-- ANCHOR_END: tagging
-- ANCHOR_END: all
