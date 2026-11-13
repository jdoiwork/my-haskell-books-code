-- ANCHOR: all
-- 🗺️ The map of time: one quest, seen through the types of time.
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

-- 🕘 A time on the wall clock, with no day.
-- 壁の時計の時刻だけ。日付はない
nineAM :: TimeOfDay
nineAM = TimeOfDay 9 0 0

-- 🗓️ A day and a time: the wall clock, with no place.
-- 日付と時刻：壁の時計。どこの壁かは持たない
postedLocal :: LocalTime
postedLocal = LocalTime postedDay nineAM
-- ANCHOR_END: calendar

-- ANCHOR: moment
-- 🌍 One moment for the whole world, counted in UTC.
-- 世界共通の日時。UTC で数える
postedUtc :: UTCTime
postedUtc = UTCTime postedDay (9 * 60 * 60)
-- ANCHOR_END: moment
-- ANCHOR_END: all
