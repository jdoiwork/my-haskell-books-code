-- Answers for the training of the "typed SQL" chapter.
-- 「型で SQL を守る」の章のトレーニングの解答
-- ANCHOR: imports
{-# LANGUAGE OverloadedLabels #-}
module TypedAnswers where

import Database.Selda

import TypedGuild
-- ANCHOR_END: imports

-- ANCHOR: byJob
-- 🧑‍🤝‍🧑 Every adventurer with this job, by name.
-- この職業の冒険者全員（名前の順）
byJob :: Text -> Query s (Row s Adventurer)
byJob jobName = do
  a <- select adventurers
  restrict (a ! #job .== literal jobName)
  order (a ! #name) ascending
  pure a
-- ANCHOR_END: byJob

-- ANCHOR: jobCounts
-- 🔢 How many adventurers each job has.
-- 職業ごとの人数
jobCounts :: Query s (Col s Text :*: Col s Int)
jobCounts = aggregate $ do
  a <- select adventurers
  j <- groupBy (a ! #job)
  pure (j :*: count (a ! #advId))
-- ANCHOR_END: jobCounts
