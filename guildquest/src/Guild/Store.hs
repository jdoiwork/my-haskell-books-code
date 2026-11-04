-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}

-- ANCHOR: module
-- 🗄️ The guild's database: adventurers, quests, and gold.
-- ギルドのデータベース：冒険者、依頼、ゴールド
module Guild.Store
  ( addMember
  , post
  , findQuest
  , takeQuest
  , completeQuest
  , questBoard
  , members
  ) where

-- SQLite.
-- SQLite
import Database.SQLite.Simple

-- Text, and the first element of a list.
-- テキストと、リストの最初の要素
import Data.Text  (Text)
import Data.Maybe (listToMaybe)

-- The guild's types and rules.
-- ギルドの型と規則
import Guild.Quest
-- ANCHOR_END: module

-- ANCHOR: create
-- 🧙 A new member joins the guild (level 1, no gold yet).
-- 新しい冒険者がギルドに入る（レベル 1、所持金 0）
addMember :: Connection -> Text -> Text -> IO ()
addMember conn who jobName =
  execute conn
    "INSERT INTO adventurers (name, job, level) VALUES (?, ?, 1)"
    (who, jobName)

-- 📌 Post a new quest on the board. Returns its number.
-- 依頼板に、新しい依頼を貼る。依頼の番号を返す
post :: Connection -> Text -> Int -> IO Int
post conn questTitle gold = do
  execute conn "INSERT INTO quests (title, reward) VALUES (?, ?)"
    (questTitle, gold)
  qid <- lastInsertRowId conn
  pure (fromIntegral qid)
-- ANCHOR_END: create

-- ANCHOR: take
-- 🔍 The quest with this number, if any.
-- この番号の依頼（あれば）
findQuest :: Connection -> Int -> IO (Maybe Quest)
findQuest conn qid = do
  rows <- query conn
    "SELECT id, title, reward, taken_by, done FROM quests WHERE id = ?"
    (Only qid)
  pure (listToMaybe rows)

-- ✋ An adventurer takes a quest that no one has taken yet.
--    Returns False if the adventurer or the open quest is not there.
-- まだ誰も受けていない依頼を、冒険者が受ける
-- 冒険者か、受けられる依頼がなければ False を返す
takeQuest :: Connection -> Int -> Text -> IO Bool
takeQuest conn qid who = do
  ids <- query conn "SELECT id FROM adventurers WHERE name = ?" (Only who)
  case ids of
    [Only adventurer] -> do
      execute conn
        "UPDATE quests SET taken_by = ? \
        \WHERE id = ? AND taken_by IS NULL AND done = 0"
        (adventurer :: Int, qid)
      changed <- changes conn
      pure (changed == 1)
    _ -> pure False
-- ANCHOR_END: take

-- ANCHOR: complete
-- 🏁 Complete a quest and pay the reward, all or nothing.
-- 依頼を完了にして、報酬を払う。全部か無しか
completeQuest :: Connection -> Int -> IO (Either QuestError Int)
completeQuest conn qid =
  withTransaction conn $ do
    found <- findQuest conn qid
    case completion qid found of
      Left err -> pure (Left err)
      Right (who, gold) -> do
        execute conn "UPDATE quests SET done = 1 WHERE id = ?" (Only qid)
        execute conn
          "UPDATE adventurers SET gold = gold + ? WHERE id = ?"
          (gold, who)
        pure (Right gold)
-- ANCHOR_END: complete

-- ANCHOR: board
-- 📋 The quest board: every quest, and who took it (if anyone).
-- 依頼板：すべての依頼と、受けた冒険者（いれば）
questBoard :: Connection -> IO [BoardLine]
questBoard conn = query_ conn """
  SELECT q.id, q.title, q.reward, a.name, q.done
  FROM quests AS q
  LEFT JOIN adventurers AS a ON a.id = q.taken_by
  ORDER BY q.id
  """

-- 🧙 Every member and their gold, the richest first.
-- 全員と所持金。お金持ちの順
members :: Connection -> IO [Member]
members conn = query_ conn
  "SELECT name, job, gold FROM adventurers ORDER BY gold DESC, name"
-- ANCHOR_END: board
-- ANCHOR_END: all
