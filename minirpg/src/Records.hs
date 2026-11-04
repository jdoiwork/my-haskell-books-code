-- ANCHOR: all
-- ANCHOR: pragma
-- Let us write Text values as "..." literals.
-- "..." の書き方で Text の値を書けるようにする
{-# LANGUAGE OverloadedStrings #-}
-- Let us write record.hero to read a field.
-- record.hero のようにフィールドを読めるようにする
{-# LANGUAGE OverloadedRecordDot #-}
-- ANCHOR_END: pragma

-- ANCHOR: module
-- The hall of fame: battle records, as plain lines of text.
-- Everything here is pure.
-- 殿堂入りの記録：戦いの記録を、ただのテキストの行として扱う
-- ここはすべて純粋
module Records where

-- Text: efficient text. T.xxx: functions for Text.
-- Text：効率のよい文字列。T.xxx：Text 用の関数
import Data.Text (Text)
import Data.Text qualified as T
-- ANCHOR_END: module

-- ANCHOR: types
-- 🏁 How a battle ended.
-- バトルの結末
data Outcome = Won | Lost | Escaped
  deriving (Show, Eq, Ord, Enum, Bounded)

-- 📜 One line of the hall of fame: who, and how it ended.
-- 殿堂の1行：誰が、どんな結末だったか
data Record = Record
  { hero    :: Text
  , outcome :: Outcome
  }
  deriving (Show, Eq)
-- ANCHOR_END: types

-- ANCHOR: outcomeText
-- 🔤 An outcome as text, and back.
-- 結末をテキストにする。また、テキストから結末を読む
outcomeText :: Outcome -> Text
outcomeText Won     = "won"
outcomeText Lost    = "lost"
outcomeText Escaped = "escaped"

parseOutcome :: Text -> Maybe Outcome
parseOutcome "won"     = Just Won
parseOutcome "lost"    = Just Lost
parseOutcome "escaped" = Just Escaped
parseOutcome _         = Nothing
-- ANCHOR_END: outcomeText

-- ANCHOR: formatRecord
-- 📝 A record as one line of the file: "Lambda,won"
-- 記録をファイルの1行にする："Lambda,won"
formatRecord :: Record -> Text
formatRecord record = record.hero <> "," <> outcomeText record.outcome

-- 🔍 Read one line of the file. A broken line gives Nothing.
-- ファイルの1行を読む。壊れた行なら Nothing
parseRecord :: Text -> Maybe Record
parseRecord line =
  case T.splitOn "," line of
    [name, result] ->
      case parseOutcome result of
        Just o  -> Just (Record { hero = name, outcome = o })
        Nothing -> Nothing
    _ -> Nothing
-- ANCHOR_END: formatRecord

-- ANCHOR: summary
-- 📊 A one-line summary: "3 battles, 2 won"
-- 1行のまとめ："3 battles, 2 won"
summary :: [Record] -> Text
summary records =
  let battles = length records
      wins    = length (filter (\r -> r.outcome == Won) records)
  -- 「〇〇回の戦い、〇〇回の勝ち」
  in  T.pack (show battles) <> " battles, "
        <> T.pack (show wins) <> " won"
-- ANCHOR_END: summary

-- ANCHOR: removeHero
-- 🧹 Remove every record of this hero.
-- この勇者の記録を、すべて取り除く
removeHero :: Text -> [Record] -> [Record]
removeHero name records = filter (\r -> r.hero /= name) records
-- ANCHOR_END: removeHero
-- ANCHOR_END: all
