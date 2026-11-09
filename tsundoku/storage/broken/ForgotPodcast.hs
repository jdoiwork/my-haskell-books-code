-- ANCHOR: all
-- | Step 1, before the fix: the new kind is not in `entryIds` yet.
--   GHC (with -Wall) points at the missing case.
-- ステップ1の直す前：新しい種類を、entryIds にまだ書いていない。
-- GHC（-Wall）が、足りない場合を指さす
module ForgotPodcast
  ( Contents (..)
  , entryIds
  ) where

import Data.Tree (flatten)

import Tsundoku.Types hiding (Contents (..), entryIds)

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
  where
    idsOf toc = map entryId (concatMap flatten toc)
-- ANCHOR_END: podcast
-- ANCHOR_END: all
