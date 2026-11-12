-- ANCHOR: all
-- ⏳ Lengths of time: seconds on the line of moments, and months and
--    days on the calendar.
-- 時間の長さ：瞬間の線の上の秒と、暦の上の月と日
-- ANCHOR: module
module Spans where

import Data.Time
  ( CalendarDiffDays (..)
  , Day
  , NominalDiffTime
  , UTCTime
  , addGregorianDurationClip
  , addUTCTime
  , defaultTimeLocale
  , diffGregorianDurationClip
  , diffUTCTime
  , formatTime
  , nominalDay
  )
-- ANCHOR_END: module

-- ANCHOR: moments
-- ⌛ A rush quest must be done within 72 hours of being posted.
-- 急ぎの依頼は、貼ってから 72 時間以内に終わらせる
rushLimit :: UTCTime -> UTCTime
rushLimit posted = addUTCTime (3 * nominalDay) posted

-- ⏱️ How much time is left (a NominalDiffTime, in seconds).
-- 残り時間（NominalDiffTime。秒で数える）
timeLeft :: UTCTime -> UTCTime -> NominalDiffTime
timeLeft now limit = diffUTCTime limit now
-- ANCHOR_END: moments

-- ANCHOR: showLeft
-- 😵 "26:5:7": the minutes and seconds are not padded with 0.
-- 分と秒が 0 で埋まらない
showLeftCareless :: NominalDiffTime -> String
showLeftCareless left = formatTime defaultTimeLocale "%h:%M:%S" left

-- 🕰️ "26:05:07": %h is all the hours; %0M and %0S pad with 0.
-- %h は時間の合計。%0M と %0S は 0 で埋める
showLeft :: NominalDiffTime -> String
showLeft left = formatTime defaultTimeLocale "%h:%0M:%0S" left
-- ANCHOR_END: showLeft

-- ANCHOR: calendarSpan
-- 🎓 A trainee's term: one month and ten days, on the calendar.
-- 見習いの期間：暦の上で、1か月と10日
traineeTerm :: CalendarDiffDays
traineeTerm = CalendarDiffDays 1 10

-- 🎓 The last day of the term (a missing day is clipped to the month's end).
-- 見習いの期間が終わる日（その月にない日は、月末に切り詰める）
termEnds :: Day -> Day
termEnds joined = addGregorianDurationClip traineeTerm joined

-- 📏 How long a member has served, in months and days.
-- メンバーになってから、何か月と何日たったか
servedFor :: Day -> Day -> CalendarDiffDays
servedFor today joined = diffGregorianDurationClip today joined
-- ANCHOR_END: calendarSpan
-- ANCHOR_END: all
