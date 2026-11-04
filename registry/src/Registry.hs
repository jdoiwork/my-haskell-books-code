{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- ANCHOR: module
-- 📋 The guild's registration desk: check an application,
--    and either register the adventurer or say why not.
-- ギルドの登録受付：申込書を調べて、登録するか、断る理由を伝える
module Registry where

import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)
-- ANCHOR_END: module

-- ANCHOR: types
-- 🎖️ The jobs the guild accepts.
-- ギルドが受け付ける職業
data Job = Warrior | Mage | Thief | Healer
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- 🧑 A registered adventurer.
-- 登録された冒険者
data Adventurer = Adventurer
  { name :: Text
  , age  :: Int
  , job  :: Job
  }
  deriving (Show, Eq)
-- ANCHOR_END: types

-- ANCHOR: errors
-- ❌ Every reason an application can be turned down.
-- 申込書を断る理由、すべて
data RegistrationError
  = EmptyName           -- the name is empty / 名前が空
  | BadAge Text         -- the age is not a number / 年齢が数でない
  | TooYoung Int        -- under 15 / 15歳未満
  | UnknownJob Text     -- not one of the jobs / 知らない職業
  | BadLine Text        -- not "name,age,job" / 形がおかしい行
  deriving (Show, Eq)
-- ANCHOR_END: errors

-- ANCHOR: validators
-- 🔍 The name must not be empty.
-- 名前は空ではいけない
validateName :: Text -> Either RegistrationError Text
validateName raw
  | T.null n  = Left EmptyName
  | otherwise = Right n
  where
    n = T.strip raw

-- 🔍 The age must be a number, and 15 or more.
-- 年齢は数で、15歳以上
validateAge :: Text -> Either RegistrationError Int
validateAge raw =
  case readMaybe (T.unpack (T.strip raw)) of
    Nothing -> Left (BadAge raw)
    Just a
      | a < 15    -> Left (TooYoung a)
      | otherwise -> Right a

-- 🔍 The job must be one the guild knows.
-- 職業は、ギルドが知っているもの
validateJob :: Text -> Either RegistrationError Job
validateJob raw =
  case readMaybe (T.unpack (T.strip raw)) of
    Nothing -> Left (UnknownJob raw)
    Just j  -> Right j
-- ANCHOR_END: validators

-- ANCHOR: stairs
-- 🪜 Check the three parts, one after another, with case.
--    It works, but the code walks down a staircase.
-- 3つの部分を順に case で調べる
-- 動くけれど、コードが階段のように右へ下りていく
registerStairs :: Text -> Text -> Text -> Either RegistrationError Adventurer
registerStairs rawName rawAge rawJob =
  case validateName rawName of
    Left err -> Left err
    Right n ->
      case validateAge rawAge of
        Left err -> Left err
        Right a ->
          case validateJob rawJob of
            Left err -> Left err
            Right j  -> Right (Adventurer { name = n, age = a, job = j })
-- ANCHOR_END: stairs

-- ANCHOR: register
-- 🚂 The same checks with do: the first Left stops everything.
-- 同じチェックを do で書く。最初の Left で、そこで止まる
registerParts :: Text -> Text -> Text -> Either RegistrationError Adventurer
registerParts rawName rawAge rawJob = do
  n <- validateName rawName
  a <- validateAge rawAge
  j <- validateJob rawJob
  pure (Adventurer { name = n, age = a, job = j })

-- 📄 One line of the application file: "name,age,job".
-- 申込書のファイルの1行："名前,年齢,職業"
register :: Text -> Either RegistrationError Adventurer
register line =
  case T.splitOn "," line of
    [rawName, rawAge, rawJob] -> registerParts rawName rawAge rawJob
    _                         -> Left (BadLine line)
-- ANCHOR_END: register

-- ANCHOR: allProblems
-- 📝 Not just the first problem: every problem of an application.
-- 最初の問題だけでなく、申込書のすべての問題
allProblems :: Text -> Text -> Text -> [RegistrationError]
allProblems rawName rawAge rawJob =
  problem (validateName rawName)
    ++ problem (validateAge rawAge)
    ++ problem (validateJob rawJob)

-- ❓ A Left becomes a one-item list; a Right becomes an empty list.
-- Left は1つだけのリストに、Right は空のリストにする
problem :: Either e a -> [e]
problem (Left err) = [err]
problem (Right _)  = []
-- ANCHOR_END: allProblems

-- ANCHOR: message
-- 💬 A message for each error, for the clerk to read.
-- 受付係が読むための、エラーごとのメッセージ
message :: RegistrationError -> Text
message err =
  case err of
    EmptyName      -> "the name is empty"
    BadAge raw     -> "the age is not a number: " <> raw
    TooYoung a     -> "too young: " <> T.pack (show a) <> " (15 or more)"
    UnknownJob raw -> "unknown job: " <> raw
    BadLine raw    -> "not \"name,age,job\": " <> raw
-- ANCHOR_END: message

-- ANCHOR: registerAll
-- 📚 Register many lines at once: the failures and the adventurers.
-- たくさんの行をまとめて登録する。失敗と冒険者に分ける
registerAll :: [Text] -> ([RegistrationError], [Adventurer])
registerAll ls = partitionEithers (map register ls)
-- ANCHOR_END: registerAll
-- ANCHOR_END: all
