-- Answers for the training of the database chapter.
-- データベース入門の章のトレーニングの解答
-- ANCHOR: header
{-# LANGUAGE OverloadedStrings #-}
module RosterAnswers where

import Database.SQLite.Simple
import Data.Text (Text)

import Roster (Adventurer)
-- ANCHOR_END: header

-- ANCHOR: count
-- 🔢 How many adventurers are in the roster?
-- 名簿に、何人いる？
countAdventurers :: Connection -> IO Int
countAdventurers conn = do
  rows <- query_ conn "SELECT COUNT(*) FROM adventurers"
  case rows of
    [Only n] -> pure n
    _        -> pure 0
-- ANCHOR_END: count

-- ANCHOR: byJob
-- 🧑‍🤝‍🧑 Every adventurer with this job, in the order they joined.
-- この職業の冒険者全員（登録した順）
adventurersByJob :: Connection -> Text -> IO [Adventurer]
adventurersByJob conn jobName =
  query conn
    "SELECT id, name, job, level FROM adventurers WHERE job = ? ORDER BY id"
    (Only jobName)
-- ANCHOR_END: byJob

-- ANCHOR: top
-- 🏆 The n strongest adventurers: the highest level first, then by name.
-- 強い順に n 人：レベルの高い順、同じなら名前の順
topAdventurers :: Connection -> Int -> IO [Adventurer]
topAdventurers conn n =
  query conn sql (Only n)
  where
    sql = "SELECT id, name, job, level FROM adventurers \
          \ORDER BY level DESC, name LIMIT ?"
-- ANCHOR_END: top
