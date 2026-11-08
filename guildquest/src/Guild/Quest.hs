-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedRecordDot #-}

-- ANCHOR: module
-- 📜 Quests and the guild's rules. Everything here is pure.
-- 依頼と、ギルドの規則。ここはすべて純粋
module Guild.Quest
  ( Quest (..)
  , BoardLine (..)
  , Member (..)
  , QuestError (..)
  , completion
  ) where

-- Turning a row into a value.
-- 行を値にする
import Database.SQLite.Simple (FromRow (..), field)

-- Text.
-- テキスト
import Data.Text (Text)
-- ANCHOR_END: module
-- ANCHOR_END: head

-- ANCHOR: types
-- 📜 A quest, as read from the quests table.
-- 依頼の表から読んだ依頼
data Quest = Quest
  { questId :: Int
  , title   :: Text
  , reward  :: Int
    -- who took it (Nothing: no one yet)
    -- 受けた冒険者の番号（Nothing：まだ誰も受けていない）
  , takenBy :: Maybe Int
  , done    :: Bool
  }
  deriving (Show, Eq)

-- 📋 One line of the quest board: a quest and who took it.
-- 依頼板の1行：依頼と、受けた冒険者の名前
data BoardLine = BoardLine
  { lineId     :: Int
  , lineTitle  :: Text
  , lineReward :: Int
    -- who took it (Nothing: no one yet)
    -- 受けた冒険者の名前（Nothing：まだ誰も受けていない）
  , taker      :: Maybe Text
  , lineDone   :: Bool
  }
  deriving (Show, Eq)

-- 🧙 A member of the guild, with the gold they have.
-- ギルドの一員と、その所持金
data Member = Member
  { memberName :: Text
  , memberJob  :: Text
  , gold       :: Int
  }
  deriving (Show, Eq)

instance FromRow Quest where
  fromRow = Quest <$> field <*> field <*> field <*> field <*> field

instance FromRow BoardLine where
  fromRow = BoardLine <$> field <*> field <*> field <*> field <*> field

instance FromRow Member where
  fromRow = Member <$> field <*> field <*> field
-- ANCHOR_END: types

-- ANCHOR: rule
-- 🚫 Why a quest cannot be completed.
-- 依頼を完了にできない理由
data QuestError
  = NoSuchQuest Int    -- there is no such quest / その依頼はない
  | NotTaken Int       -- no one took it / 誰も受けていない
  | AlreadyDone Int    -- it is already done / もう完了している
  deriving (Show, Eq)

-- ⚖️ The guild's rule: can this quest be completed?
--    If so, who gets how much gold.
-- ギルドの規則：この依頼を完了にしてよいか
-- よいなら、誰に何ゴールドを払うか
completion :: Int -> Maybe Quest -> Either QuestError (Int, Int)
completion qid found =
  case found of
    Nothing -> Left (NoSuchQuest qid)
    Just q
      | q.done -> Left (AlreadyDone qid)
      | otherwise ->
          case q.takenBy of
            Nothing  -> Left (NotTaken qid)
            Just who -> Right (who, q.reward)
-- ANCHOR_END: rule
-- ANCHOR_END: all
