-- ANCHOR: head
-- | Training, step 2: find the first entry I have not read yet.
-- トレーニングのステップ2：まだ読んでいない、いちばん前の項目を探す
module ShelfAnswers.FirstUnread
  ( firstUnread
  ) where

import Data.List (find)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree (flatten)

import Tsundoku.Types
-- ANCHOR_END: head

-- ANCHOR: first
-- 👉 The first entry, from top to bottom, that is not read yet.
--    Nothing when everything is read.
-- 上から順に見て、まだ読んでいない最初の項目。全部読んでいたら Nothing
firstUnread :: Set EntryId -> Toc u -> Maybe (Entry u)
firstUnread finished toc = find unread (concatMap flatten toc)
  where
    unread entry = not (entryId entry `Set.member` finished)
-- ANCHOR_END: first
