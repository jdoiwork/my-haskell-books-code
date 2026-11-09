-- ANCHOR: all
-- | Step 1: add podcasts. A copy of `Contents` with one more kind,
--   so that the chapter's own code stays as it is.
-- ステップ1：ポッドキャストを足す。章のコードはそのままにしたいので、
-- Contents を写して、種類を1つ足したもの
-- ANCHOR: head
module ShelfAnswers.Podcast
  ( Contents (..)
  , entryIds
  ) where

import Data.Tree (flatten)

import Tsundoku.Types hiding (Contents (..), entryIds)
-- ANCHOR_END: head

-- ANCHOR: podcast
-- 🎙️ Podcasts: shows of episodes, measured in minutes.
-- ポッドキャスト：番組ごとに回が並ぶ。量は分
data Contents
  = Book    (Toc Pages)
  | Serial  (Toc Episodes)
  | Series  (Toc Minutes)
  | Film    Minutes
  | Game
  | Podcast (Toc Minutes)
  deriving (Show, Eq)

entryIds :: Contents -> [EntryId]
entryIds what = case what of
  Book    toc -> idsOf toc
  Serial  toc -> idsOf toc
  Series  toc -> idsOf toc
  Film    _   -> []
  Game        -> []
  Podcast toc -> idsOf toc
  where
    idsOf toc = map entryId (concatMap flatten toc)
-- ANCHOR_END: podcast
-- ANCHOR_END: all
