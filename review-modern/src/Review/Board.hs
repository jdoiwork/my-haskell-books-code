-- ANCHOR: all
-- | A quest board that works offline: changes are a data type, the
--   board is an OR-Set, and two devices sync side by side.
-- 圏外でも使える依頼板：変更はデータ型、板は OR-Set、
-- 2台の端末が並んで同期する
module Review.Board
  ( Change (..)
  , applyChange
  , applyAll
  , NaiveBoard (..)
  , naiveApply
  , naiveMerge
  , guildBoard
  , syncDevices
  ) where

-- ANCHOR: imports
import Control.Concurrent.Async (concurrently)
import qualified Data.Set as Set
import Data.Set (Set)

import Modern.Crdt (OrSet, Tag, add, emptySet, members, remove)
-- ANCHOR_END: imports

-- ANCHOR: change
-- 📝 Step 1: what a device can do to the board.
-- ステップ1：端末が板にできること
data Change
  = Post Tag String
  | Finish String
  deriving (Show, Eq)

-- 🔁 Apply one change, by its shape.
-- 変更を1つ、形ごとに当てはめる
applyChange :: OrSet String -> Change -> OrSet String
applyChange board (Post tag quest) = add tag quest board
applyChange board (Finish quest)   = remove quest board

applyAll :: OrSet String -> [Change] -> OrSet String
applyAll = foldl' applyChange
-- ANCHOR_END: change

-- ANCHOR: naive
-- 😵 Step 2: a first draft that keeps a plain set and merges with
--    union. A finished quest comes back after the merge.
-- ステップ2：ふつうの集合を持ち、和集合で合わせる最初の下書き。
-- 終えた依頼が、合わせると戻ってきてしまう
newtype NaiveBoard = NaiveBoard (Set String)
  deriving (Show, Eq)

naiveApply :: NaiveBoard -> Change -> NaiveBoard
naiveApply (NaiveBoard quests) (Post _ quest) =
  NaiveBoard (Set.insert quest quests)
naiveApply (NaiveBoard quests) (Finish quest) =
  NaiveBoard (Set.delete quest quests)

naiveMerge :: NaiveBoard -> NaiveBoard -> NaiveBoard
naiveMerge (NaiveBoard a) (NaiveBoard b) = NaiveBoard (Set.union a b)
-- ANCHOR_END: naive

-- ANCHOR: guildBoard
-- 📋 The board the guild starts with: slime and herb.
-- ギルドが最初に貼った板：slime と herb
guildBoard :: OrSet String
guildBoard =
  applyAll emptySet [Post ("guild", 0) "slime", Post ("guild", 1) "herb"]
-- ANCHOR_END: guildBoard

-- ANCHOR: sync
-- 📱 Step 3: two devices work at the same time from the same board,
--    then merge. concurrently waits for both, and a failure in one
--    cancels the other.
-- ステップ3：2台が同じ板から同時に作業して、合わせる。
-- concurrently は両方を待ち、片方が失敗したら、もう片方も止める
syncDevices
  :: OrSet String -> [Change] -> [Change] -> IO (Set String)
syncDevices start laptopChanges phoneChanges = do
  (laptop, phone) <-
    concurrently (pure $! applyAll start laptopChanges)
                 (pure $! applyAll start phoneChanges)
  pure (members (laptop <> phone))
-- ANCHOR_END: sync
-- ANCHOR_END: all
