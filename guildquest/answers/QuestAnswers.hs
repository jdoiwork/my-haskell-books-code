-- Answers for the training of the "growing the database" chapter.
-- 「データベースを育てる」の章のトレーニングの解答
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}
module QuestAnswers where

import Database.SQLite.Simple
import Data.Text (Text)

import Guild.Quest  (Quest)
import Guild.Schema (migrations)

-- ANCHOR: questsOf
-- 📜 The quests this adventurer took, in order.
-- この冒険者が受けた依頼（番号の順）
questsOf :: Connection -> Text -> IO [Quest]
questsOf conn who = query conn """
  SELECT q.id, q.title, q.reward, q.taken_by, q.done
  FROM quests AS q
  JOIN adventurers AS a ON a.id = q.taken_by
  WHERE a.name = ?
  ORDER BY q.id
  """ (Only who)
-- ANCHOR_END: questsOf

-- ANCHOR: rank
-- 🏅 Version 4: every quest gets a rank. Old quests become "Novice".
-- 版4：依頼にランクを足す。前からある依頼は "Novice" になる
migrationsWithRank :: [Query]
migrationsWithRank =
  migrations
    ++ ["ALTER TABLE quests ADD COLUMN rank TEXT NOT NULL DEFAULT 'Novice'"]
-- ANCHOR_END: rank

-- ANCHOR: pay
-- 🚫 Why the gold cannot be paid.
-- ゴールドを払えない理由
data PayError
  -- no one has this name / その名前の冒険者はいない
  = NoSuchMember Text
  -- they have only this much / これだけしか持っていない
  | NotEnoughGold Text Int
  deriving (Show, Eq)

-- 💰 How much gold this member has, if they are a member.
-- この冒険者の所持金（いれば）
goldOf :: Connection -> Text -> IO (Maybe Int)
goldOf conn who = do
  rows <- query conn "SELECT gold FROM adventurers WHERE name = ?" (Only who)
  case rows of
    [Only g] -> pure (Just g)
    _        -> pure Nothing

-- 🤝 Pay gold from one member to another, all or nothing.
-- ある冒険者から別の冒険者へゴールドを払う。全部か無しか
pay :: Connection -> Text -> Text -> Int -> IO (Either PayError ())
pay conn from to amount =
  withTransaction conn $ do
    payer <- goldOf conn from
    payee <- goldOf conn to
    case (payer, payee) of
      (Nothing, _) -> pure (Left (NoSuchMember from))
      (_, Nothing) -> pure (Left (NoSuchMember to))
      (Just g, Just _)
        | g < amount -> pure (Left (NotEnoughGold from g))
        | otherwise  -> do
            execute conn
              "UPDATE adventurers SET gold = gold - ? WHERE name = ?"
              (amount, from)
            execute conn
              "UPDATE adventurers SET gold = gold + ? WHERE name = ?"
              (amount, to)
            pure (Right ())
-- ANCHOR_END: pay
