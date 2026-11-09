-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE MultilineStrings #-}
-- | Training, step 1: notes left behind by a removed work.
-- トレーニングのステップ1：消した作品に置いていかれた感想
module ShelfAnswers.Orphans (orphanNotes) where

import Database.SQLite.Simple
-- ANCHOR_END: head

-- ANCHOR: orphans
-- 👻 How many notes point at a work that is no longer there?
-- もういない作品を指している感想は、いくつ？
orphanNotes :: Connection -> IO Int
orphanNotes conn = do
  [Only n] <- query_ conn """
    SELECT count(*) FROM notes
     WHERE work_id NOT IN (SELECT id FROM works)
    """
  pure n
-- ANCHOR_END: orphans
-- ANCHOR_END: all
