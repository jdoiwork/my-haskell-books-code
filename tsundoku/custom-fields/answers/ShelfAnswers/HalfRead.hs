-- ANCHOR: head
-- | Training, step 1: count an entry I am in the middle of as half.
-- トレーニングのステップ1：読みかけの項目を、半分と数える
module ShelfAnswers.HalfRead
  ( progressWithHalf
  ) where

import Data.Set (Set)
import Data.Set qualified as Set

import Tsundoku.Progress (Progress (..))
import Tsundoku.Types
-- ANCHOR_END: head

-- ANCHOR: half
-- 🌓 Like progressOf, but an entry I am still reading counts as half
--    of its size (rounded down). Finished wins if it is in both.
-- progressOf と同じだが、読みかけの項目は、量の半分（切り捨て）と数える。
-- 両方に入っていたら、読み終えたほうを取る
progressWithHalf
  :: Amount u => Set EntryId -> Set EntryId -> Toc u -> Progress u
progressWithHalf finished halfway toc = foldMap (foldMap piece) toc
  where
    piece entry
      | i `Set.member` finished = Progress whole whole
      | i `Set.member` halfway  = Progress half  whole
      | otherwise               = Progress mempty whole
      where
        i     = entryId entry
        whole = size entry
        half  = amount (count whole `div` 2)
-- ANCHOR_END: half
