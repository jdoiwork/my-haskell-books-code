module GuildAnswers where

import Guild

-- ANCHOR: prevJob
-- 🔁 The previous job on the menu. Before the first, the last.
-- メニューの前の職業。最初の前は、最後
prevJob :: Job -> Job
prevJob job
  | job == minBound = maxBound
  | otherwise       = pred job
-- ANCHOR_END: prevJob

-- ANCHOR: parseJobs
-- 🔤 Read many job names at once.
-- たくさんの職業名を、まとめて読む
parseJobs :: [String] -> [Maybe Job]
parseJobs inputs = map parseJob inputs
-- ANCHOR_END: parseJobs

-- ANCHOR: exp
-- ⭐ Experience points that can be added like a number.
-- 数のように足せる経験値
newtype Exp = Exp Int
  deriving (Show, Eq, Ord, Num)
-- ANCHOR_END: exp

-- ANCHOR: totalGold
-- 💰 Add up a list of gold. sum works because Gold is a Num.
-- ゴールドのリストを合計する。Gold が Num なので sum が使える
totalGold :: [Gold] -> Gold
totalGold golds = sum golds
-- ANCHOR_END: totalGold
