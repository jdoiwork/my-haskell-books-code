-- Answers for the training of the stats chapter.
-- 集計する章のトレーニングの解答
module StatsAnswers where

-- Text, and Map for the stats
-- Text と、集計の Map
import Data.Text       (Text)
import Data.Map.Strict qualified as Map

-- Sort by a key
-- 基準を決めて並べ替える
import Data.List (sortOn)

-- The records and their stats
-- 記録と、その集計
import Records
import Stats

-- ANCHOR: winsIn
-- 🥇 How many times this hero won. An unknown hero won 0 times.
-- この勇者が勝った回数。記録のない勇者は 0 回
winsIn :: Text -> HeroStats -> Int
winsIn name stats =
  case Map.lookup name stats of
    Nothing     -> 0
    Just counts -> Map.findWithDefault 0 Won counts
-- ANCHOR_END: winsIn

-- ANCHOR: ranking
-- 🏆 Every hero with their wins, the most wins first.
-- 勇者と勝った回数。勝った回数が多い順
ranking :: HeroStats -> [(Text, Int)]
ranking stats =
  let wins = [ (name, winsIn name stats) | name <- Map.keys stats ]
  in  sortOn (\(_, w) -> negate w) wins
-- ANCHOR_END: ranking
