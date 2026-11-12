-- ANCHOR: all
-- 🏋️ Answers to the training of the chapter.
-- 章末のトレーニングの解答
-- ANCHOR: module
{-# LANGUAGE OverloadedRecordDot #-}
module ZoneAnswers where

import Data.Time
  ( Day
  , LocalTime (..)
  , TimeOfDay (..)
  , TimeZone
  , UTCTime
  , addDays
  , defaultTimeLocale
  , formatTime
  , localTimeOfDay
  , utcToLocalTime
  )

import Board (Quest (..))
import Branches (Branch, Reading (..), readWallClock, wallClockAt)
import Clock (Clock)
-- ANCHOR_END: module

-- ANCHOR: step1
-- 🏰 "Slay the goblins (posted 2026-11-12 16:00 EST)", in the branch's
--    wall clock.
-- 支部の壁の時計で、依頼を1行に
noticeIn :: Branch -> Quest -> String
noticeIn branch quest =
  let wall  = wallClockAt branch quest.postedAt
      shown = formatTime defaultTimeLocale "%Y-%m-%d %H:%M %Z" wall
  in quest.title <> " (posted " <> shown <> ")"
-- ANCHOR_END: step1

-- ANCHOR: step2
-- 👋 A greeting that depends on the hour of the wall clock.
-- 壁の時計の「時」で変わる、あいさつ
greeting :: Clock -> TimeZone -> IO String
greeting clock zone = do
  now <- clock
  let hour = todHour (localTimeOfDay (utcToLocalTime zone now))
  pure (greetingAt hour)

-- 🌅 Morning from 5, afternoon from 12, evening from 18.
-- 朝は 5 時から、昼は 12 時から、夜は 18 時から
greetingAt :: Int -> String
greetingAt hour
  | hour >= 5 && hour < 12  = "Good morning"
  | hour >= 12 && hour < 18 = "Good afternoon"
  | otherwise               = "Good evening"
-- ANCHOR_END: step2

-- ANCHOR: step3
-- 🗓️ The meeting at the same wall clock time, every day for n days.
--    A skipped time is left out; a time shown twice is the first one.
-- 毎日、壁の時計の同じ時刻にある会議を n 日分。
-- 飛ばされた時刻は外し、2回ある時刻は1回目にする
dailyMeetings :: Branch -> Day -> Integer -> TimeOfDay -> [UTCTime]
dailyMeetings branch start n time =
  concatMap meetingOn [addDays k start | k <- [0 .. n - 1]]
  where
    meetingOn day =
      case readWallClock branch (LocalTime day time) of
        Once moment   -> [moment]
        Twice first _ -> [first]
        Skipped       -> []
-- ANCHOR_END: step3
-- ANCHOR_END: all
