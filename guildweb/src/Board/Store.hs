-- ANCHOR: all
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- 🗄️ Where the quests are kept: in memory, or in PostgreSQL.
-- 依頼の置き場：メモリーか、PostgreSQL か
module Board.Store
  ( QuestStore (..)
  , memoryStore
  , postgresStore
  ) where

import Data.IORef
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple

import Board.Types

-- ANCHOR: store
-- 🗄️ The three things the server needs from a store.
-- サーバーが置き場に頼む、3つのこと
data QuestStore = QuestStore
  { listQuests  :: IO [Quest]
  , addQuest    :: NewQuest -> IO Quest
  , finishQuest :: Int -> IO (Maybe Quest)
  }
-- ANCHOR_END: store

-- ANCHOR: memory
-- 🧠 In memory: for trying things out and for the tests.
-- メモリー：試すときと、テストのため
memoryStore :: IO QuestStore
memoryStore = do
  ref <- newIORef ([] :: [Quest])
  let add new = atomicModifyIORef' ref $ \quests ->
        let quest = Quest (length quests + 1) new.title new.reward False
        in (quests <> [quest], quest)
      finish n = atomicModifyIORef' ref $ \quests ->
        let mark q = if q.questId == n then q { done = True } else q
            updated = map mark quests
        in (updated, lookup n [(q.questId, q) | q <- updated])
  pure QuestStore
    { listQuests  = readIORef ref
    , addQuest    = add
    , finishQuest = finish
    }
-- ANCHOR_END: memory

-- ANCHOR: postgres
-- 🐘 In PostgreSQL: make the table if it is not there yet.
-- PostgreSQL：表がまだなければ作る
postgresStore :: String -> IO QuestStore
postgresStore url = do
  conn <- connectPostgreSQL (BS.pack url)
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS quests (\
    \  id SERIAL PRIMARY KEY,\
    \  title TEXT NOT NULL,\
    \  reward INTEGER NOT NULL,\
    \  done BOOLEAN NOT NULL DEFAULT FALSE)"
  let toQuest (i, t, r, d) = Quest i t r d
  pure QuestStore
    { listQuests = map toQuest <$>
        query_ conn "SELECT id, title, reward, done FROM quests ORDER BY id"
    , addQuest = \new -> do
        [row] <- query conn
          "INSERT INTO quests (title, reward) VALUES (?, ?) \
          \RETURNING id, title, reward, done"
          (new.title, new.reward)
        pure (toQuest row)
    , finishQuest = \n -> do
        rows <- query conn
          "UPDATE quests SET done = TRUE WHERE id = ? \
          \RETURNING id, title, reward, done"
          (Only n)
        pure (toQuest <$> safeHead rows)
    }
  where
    safeHead (x : _) = Just x
    safeHead []      = Nothing
-- ANCHOR_END: postgres
-- ANCHOR_END: all
