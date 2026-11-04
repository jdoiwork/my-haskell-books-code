-- ANCHOR: all
-- ANCHOR: pragma
-- "..." as Text, #name as a column, and deriving with anyclass.
-- "..." を Text に、#name を列に、anyclass で資格を作る
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}
-- ANCHOR_END: pragma

-- ANCHOR: module
-- 🛡️ The guild with selda: queries are Haskell, checked by types.
-- selda で書いたギルド：問い合わせを Haskell で書き、型で確かめる
module TypedGuild
  ( Adventurer (..)
  , Quest (..)
  , adventurers
  , quests
  , addAdventurer
  , strong
  , levelUp
  , removeAdventurer
  , postQuest
  , questBoard
  ) where

-- selda: tables, queries, and the SQL they become.
-- selda：表、問い合わせ、それが変わる SQL
import Database.Selda
-- ANCHOR_END: module

-- ANCHOR: types
-- 🧙 An adventurer. The record is the table.
-- 冒険者。このレコードが、そのまま表になる
data Adventurer = Adventurer
  { advId :: ID Adventurer
  , name  :: Text
  , job   :: Text
  , level :: Int
  }
  deriving (Show, Eq, Generic, SqlRow)

-- 📜 A quest, and who took it (Nothing: no one yet).
-- 依頼と、受けた冒険者（Nothing：まだ誰も受けていない）
data Quest = Quest
  { questId :: ID Quest
  , title   :: Text
  , reward  :: Int
  , takenBy :: Maybe (ID Adventurer)
  }
  deriving (Show, Eq, Generic, SqlRow)
-- ANCHOR_END: types

-- ANCHOR: tables
-- 🗄️ The tables: their names, and some rules for the columns.
-- 表：表の名前と、列の決まりごと
adventurers :: Table Adventurer
adventurers =
  table "adventurers" [#advId :- autoPrimary, #name :- unique]

quests :: Table Quest
quests = table "quests" [#questId :- autoPrimary]
-- ANCHOR_END: tables

-- ANCHOR: create
-- ➕ Create: add an adventurer. Returns the new id.
-- 登録：冒険者を足す。新しい番号を返す
addAdventurer :: Text -> Text -> Int -> SeldaM b (ID Adventurer)
addAdventurer who jobName lv =
  insertWithPK adventurers [Adventurer def who jobName lv]
-- ANCHOR_END: create

-- ANCHOR: read
-- 💪 Read: adventurers of this level or more, the strongest first.
-- 一覧：このレベル以上の冒険者を、強い順に
strong :: Int -> Query s (Row s Adventurer)
strong lv = do
  a <- select adventurers
  restrict (a ! #level .>= literal lv)
  order (a ! #level) descending
  pure a
-- ANCHOR_END: read

-- ANCHOR: update
-- ⬆️ Update: one level up. Returns how many rows changed.
-- 更新：レベルを1上げる。変わった行の数を返す
levelUp :: Text -> SeldaM b Int
levelUp who =
  update adventurers
    (\a -> a ! #name .== literal who)
    (\a -> a `with` [#level := a ! #level + 1])

-- 🚪 Delete: remove from the roster. Returns how many rows changed.
-- 削除：名簿から外す。変わった行の数を返す
removeAdventurer :: Text -> SeldaM b Int
removeAdventurer who =
  deleteFrom adventurers (\a -> a ! #name .== literal who)
-- ANCHOR_END: update

-- ANCHOR: board
-- 📌 Post a quest. Who took it can be given at once (or Nothing).
-- 依頼を貼る。受けた冒険者も一緒に渡せる（いなければ Nothing）
postQuest :: Text -> Int -> Maybe (ID Adventurer) -> SeldaM b (ID Quest)
postQuest what gold who =
  insertWithPK quests [Quest def what gold who]

-- 📋 The quest board: each quest, and the name of who took it.
--    LEFT JOIN: the name is Nothing when no one took the quest.
-- 依頼板：依頼の題と、受けた冒険者の名前
-- LEFT JOIN なので、誰も受けていなければ名前は Nothing
questBoard :: Query s (Col s Text :*: Col s (Maybe Text))
questBoard = do
  q <- select quests
  a <- leftJoin (\a -> just (a ! #advId) .== q ! #takenBy)
                (select adventurers)
  pure (q ! #title :*: a ? #name)
-- ANCHOR_END: board
-- ANCHOR_END: all
