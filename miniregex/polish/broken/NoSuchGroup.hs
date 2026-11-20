-- ANCHOR: all
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- A group name with a typo: the type of the match has no such field.
-- グループの名前を打ちまちがえた。一致の型に、そのフィールドはない
module NoSuchGroup where

import Data.Text (Text)

import MiniRegex.Quote (typed)
import MiniRegex.Typed qualified as Typed

-- 📅 The year of the first date in a line.
-- 1行の中の、最初の日付の年
yearOf :: Text -> Maybe Text
yearOf line = do
  m <- Typed.find [typed|(?<year>\d{4})-(?<month>\d{2})|] line
  pure m.yaer
-- ANCHOR_END: all
