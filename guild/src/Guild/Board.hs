-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
-- 📋 The quest board: quests grouped by rank.
-- 依頼板：依頼をランクごとに並べる
module Guild.Board
  ( boardLines
  , totalReward
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Guild.Gold (Gold, addGold, noGold)
import Guild.Quest (Quest (..), Rank)

-- 🗂️ Group the quests by rank. (Not exported: a helper.)
-- 依頼をランクごとにまとめる（公開しない、内部の道具）
byRank :: [Quest] -> Map Rank [Quest]
byRank quests =
  Map.fromListWith (\new old -> old ++ new) [ (q.rank, [q]) | q <- quests ]

-- 🖨️ The lines of the board, from Novice to Hero.
-- 依頼板の行。Novice から Hero の順
boardLines :: [Quest] -> [Text]
boardLines quests =
  concat [ section r qs | (r, qs) <- Map.toList (byRank quests) ]
  where
    section r qs = ("== " <> T.pack (show r) <> " ==") : map line qs
    line q = "- " <> q.title <> " (" <> T.pack (show q.reward) <> ")"

-- 💰 The total reward of all the quests.
-- すべての依頼の報酬の合計
totalReward :: [Quest] -> Gold
totalReward quests = foldr addGold noGold (map (.reward) quests)
-- ANCHOR_END: all
