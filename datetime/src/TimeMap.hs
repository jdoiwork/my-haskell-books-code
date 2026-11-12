-- ANCHOR: all
-- 🗺️ The map of time: one quest, seen through every type of time.
-- 時間の地図：1つの依頼を、時間のいろいろな型で見る
-- ANCHOR: module
module TimeMap where

import Data.Time
-- ANCHOR_END: module

-- ANCHOR: calendar
-- 📅 A day on the calendar, with no time of day.
-- 暦の上の日付だけ。時刻はない
postedDay :: Day
postedDay = fromGregorian 2026 11 13

-- 🕕 A time on the wall clock, with no day.
-- 壁の時計の時刻だけ。日付はない
sixAM :: TimeOfDay
sixAM = TimeOfDay 6 0 0

-- 🗓️ A day and a time: the wall clock, with no time zone.
-- 日付と時刻：壁の時計。時間帯はない
postedLocal :: LocalTime
postedLocal = LocalTime postedDay sixAM
-- ANCHOR_END: calendar

-- ANCHOR: zone
-- 🗾 The time zone of Japan: 9 hours (540 minutes) ahead of UTC.
-- 日本の時間帯：UTC より 9 時間（540 分）進んでいる
jst :: TimeZone
jst = TimeZone (9 * 60) False "JST"

-- 📌 The wall clock, together with its time zone.
-- 壁の時計と、その時間帯の組
postedZoned :: ZonedTime
postedZoned = ZonedTime postedLocal jst
-- ANCHOR_END: zone

-- ANCHOR: moment
-- 🌍 One moment for the whole world.
-- 世界で1つの瞬間
postedUtc :: UTCTime
postedUtc = zonedTimeToUTC postedZoned
-- ANCHOR_END: moment
-- ANCHOR_END: all
