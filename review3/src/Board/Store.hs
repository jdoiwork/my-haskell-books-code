-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

-- 🗄️ The quest board in SQLite.
-- 依頼の掲示板を SQLite に置く
module Board.Store
  ( Quest (..)
  , TakeError (..)
  , migrate
  , listQuests
  , postQuest
  , takeQuest
  ) where

import Control.Monad          (forM_)
import Data.Aeson             (ToJSON)
import Data.Text              (Text)
import Data.Text              qualified as T
import Database.SQLite.Simple
import GHC.Generics           (Generic)

import Board.Title

-- ANCHOR: quest
-- 📜 A quest on the board. Nobody has taken it while taker is Nothing.
-- 掲示板の依頼。taker が Nothing なら、まだ誰も受けていない
data Quest = Quest
  { questId :: Int
  , title   :: Text
  , reward  :: Int
  , taker   :: Maybe Text
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance FromRow Quest where
  fromRow = Quest <$> field <*> field <*> field <*> field
-- ANCHOR_END: quest

-- ANCHOR: migrate
-- ⬆️ The steps that grow the database, and running the missing ones.
-- データベースを育てる段と、まだ通っていない段を動かすこと
migrations :: [Query]
migrations =
  [ -- 1: the quests / 依頼
    """
    CREATE TABLE quests
      ( id     INTEGER PRIMARY KEY
      , title  TEXT    NOT NULL
      , reward INTEGER NOT NULL
      )
    """
  , -- 2: who took the quest / 誰が受けたか
    "ALTER TABLE quests ADD COLUMN taker TEXT"
  ]

migrate :: Connection -> IO ()
migrate conn = do
  [Only current] <- query_ conn "PRAGMA user_version"
  forM_ (drop current (zip [1 :: Int ..] migrations)) $ \(version, step) ->
    withTransaction conn $ do
      execute_ conn step
      let setVersion = "PRAGMA user_version = " <> T.pack (show version)
      execute_ conn (Query setVersion)
-- ANCHOR_END: migrate

-- ANCHOR: crud
-- 📋 Every quest, in the order they were posted.
-- すべての依頼（貼り出した順）
listQuests :: Connection -> IO [Quest]
listQuests conn =
  query_ conn "SELECT id, title, reward, taker FROM quests ORDER BY id"

-- 📌 Post a new quest. The values go into ? holes.
-- 新しい依頼を貼り出す。値は ? の穴へ
postQuest :: Connection -> QuestTitle -> Int -> IO Quest
postQuest conn t gold = do
  execute conn "INSERT INTO quests (title, reward) VALUES (?, ?)"
    (titleText t, gold)
  newId <- lastInsertRowId conn
  pure (Quest (fromIntegral newId) (titleText t) gold Nothing)

-- 🚫 Why a quest could not be taken.
-- 依頼を受けられなかった理由
data TakeError = NoSuchQuest | AlreadyTaken
  deriving (Show, Eq)

-- 🤝 Take a quest: look and write in one transaction, so that two
--    adventurers can not both take it.
-- 依頼を受ける。見ることと書くことを1つのトランザクションにして、
-- 2人が同時に受けられないようにする
takeQuest :: Connection -> Int -> Text -> IO (Either TakeError Quest)
takeQuest conn qid who =
  withTransaction conn $ do
    found <- query conn
      "SELECT id, title, reward, taker FROM quests WHERE id = ?" (Only qid)
    case found of
      [] -> pure (Left NoSuchQuest)
      (q : _) -> case taker q of
        Just _  -> pure (Left AlreadyTaken)
        Nothing -> do
          execute conn "UPDATE quests SET taker = ? WHERE id = ?" (who, qid)
          pure (Right q { taker = Just who })
-- ANCHOR_END: crud
-- ANCHOR_END: all
