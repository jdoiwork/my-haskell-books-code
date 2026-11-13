-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}
-- 📋 The quest board of the guild.
-- ギルドの依頼の掲示板
-- ANCHOR: module
module Board
  ( Quest (..)
  , postCareless
  , postIn
  , noticeLine
  ) where

import Data.Time
  ( Day
  , TimeZone
  , UTCTime (..)
  , addDays
  , defaultTimeLocale
  , formatTime
  , localDay
  , utcToLocalTime
  , utcToZonedTime
  )
-- ANCHOR_END: module

-- ANCHOR: quest
-- 📜 A quest on the board.
-- 掲示板の依頼
data Quest = Quest
  { title    :: String
  , postedAt :: UTCTime -- the moment it was posted / 貼り出した日時
  , postedOn :: Day     -- the day on the board    / 掲示板に出す日付
  , dueOn    :: Day     -- the last day            / 締め切りの日
  }
  deriving (Show, Eq)
-- ANCHOR_END: quest

-- ANCHOR: careless
-- 😵 Post a quest. "Today" is the day of the UTC time.
-- 依頼を貼る。「今日」は UTC の時刻の日付
postCareless :: String -> UTCTime -> Quest
postCareless name now =
  let today = utctDay now
  in Quest
       { title    = name
       , postedAt = now
       , postedOn = today
       , dueOn    = addDays 7 today
       }
-- ANCHOR_END: careless

-- ANCHOR: postIn
-- 📌 Post a quest. "Today" is the day on the wall clock of the zone.
-- 依頼を貼る。「今日」は、その時間帯の壁の時計の日付
postIn :: TimeZone -> String -> UTCTime -> Quest
postIn zone name now =
  let today = localDay (utcToLocalTime zone now)
  in Quest
       { title    = name
       , postedAt = now
       , postedOn = today
       , dueOn    = addDays 7 today
       }
-- ANCHOR_END: postIn

-- ANCHOR: notice
-- 🪧 One line for the board, in the wall clock of the zone.
--    "Slay the goblins (posted 2026-11-13 06:00 JST, due 2026-11-20)"
-- 掲示板の1行。その時間帯の壁の時計で見せる
noticeLine :: TimeZone -> Quest -> String
noticeLine zone quest =
  let posted = utcToZonedTime zone quest.postedAt
      shown  = formatTime defaultTimeLocale "%Y-%m-%d %H:%M %Z" posted
  in quest.title
       <> " (posted " <> shown
       <> ", due "    <> show quest.dueOn <> ")"
-- ANCHOR_END: notice
-- ANCHOR_END: all
