-- ANCHOR: all
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
-- | Training: the chapters a new edition added. Compare the paths of
--   headings, so that a chapter with the same name under another part
--   still counts as new.
-- トレーニング：新しい版で増えた章。見出しの道順でくらべるので、別の部の
-- 下にある同じ名前の章も、新しい章と数える
module ShelfAnswers.TocDiff (addedChapters) where

import Data.Set qualified as Set
import Data.Text (Text)

import Tsundoku.Mcp.Toc (Chapter (..))

-- ➕ The paths in the new table that the old table does not have, in the
--    order of the new table.
-- 新しい目次にあって、古い目次にない道順。新しい目次の順で
addedChapters :: [Chapter] -> [Chapter] -> [[Text]]
addedChapters old new = [ p | p <- paths new, not (Set.member p known) ]
  where
    known = Set.fromList (paths old)

-- 🚶 Every path of headings, from the top.
-- 見出しの道順の全部を、上から順に
paths :: [Chapter] -> [[Text]]
paths = go []
  where
    go above = concatMap $ \c ->
      let here = above ++ [c.heading]
      in  here : go here c.children
-- ANCHOR_END: all
