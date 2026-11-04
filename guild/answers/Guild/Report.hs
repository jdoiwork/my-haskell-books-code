-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}
-- 📊 A small report about the quests.
-- 依頼についての小さな集計
module Guild.Report
  ( rankCounts
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Guild.Quest (Quest (..), Rank)

-- 🔢 How many quests there are for each rank.
-- ランクごとの依頼の数
rankCounts :: [Quest] -> Map Rank Int
rankCounts quests = Map.fromListWith (+) [ (q.rank, 1) | q <- quests ]
-- ANCHOR_END: all
