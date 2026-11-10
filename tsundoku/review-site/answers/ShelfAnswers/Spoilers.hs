-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedRecordDot #-}
-- | Training, step 2: leave out the spoilers of works not read to the
--   end yet.
-- トレーニングのステップ2：まだ読み終えていない作品のネタバレを外す
module ShelfAnswers.Spoilers (withoutSpoilers) where

import Control.Monad (filterM)

import Tsundoku.Card (Card (..))
import Tsundoku.Search (Hit (..), Place (..))
import Tsundoku.Shelf (Shelf (..), WorkWindow (..))
import Tsundoku.Types (ReadingState (..))
-- ANCHOR_END: head

-- ANCHOR: spoilers
-- 🙈 Keep a spoiler only if its work was read to the end (Finished:
--    the same rule as visibleTo). Other hits stay.
-- ネタバレは、その作品を読み終えている（読了）ときだけ残す
-- （visibleTo と同じ決まり）。ほかのヒットは、そのまま
withoutSpoilers :: Shelf -> [Hit] -> IO [Hit]
withoutSpoilers shelf = filterM safe
  where
    safe hit = case place hit of
      InNote _ True -> do
        found <- shelf.works.getCard (hitWork hit)
        pure (maybe False readToTheEnd found)
      _             -> pure True
    readToTheEnd card = reading card == Finished
-- ANCHOR_END: spoilers
-- ANCHOR_END: all
