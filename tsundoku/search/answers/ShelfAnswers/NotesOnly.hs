-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedRecordDot #-}
-- | Training, step 1: search the notes only.
-- トレーニングのステップ1：感想だけを探す
module ShelfAnswers.NotesOnly (searchNotes, isNote) where

import Data.Text (Text)

import Tsundoku.Search (Hit (..), Place (..))
import Tsundoku.Shelf (Shelf (..))
import Tsundoku.Types (Checked)
-- ANCHOR_END: head

-- ANCHOR: notes
-- 💬 Search through the window, and keep the hits in notes.
-- 窓口で探して、感想で見つかったものだけを残す
searchNotes :: Shelf -> Text -> IO (Checked [Hit])
searchNotes shelf typed = do
  found <- shelf.searchText typed
  pure (fmap (filter (isNote . place)) found)

-- 📝 Is it a note? / 感想？
isNote :: Place -> Bool
isNote found = case found of
  InNote _ _ -> True
  _          -> False
-- ANCHOR_END: notes
-- ANCHOR_END: all
