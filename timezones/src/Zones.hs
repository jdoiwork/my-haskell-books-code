-- ANCHOR: all
-- 🗾 One quest, posted at 6 a.m. in Japan: the wall clock, the time zone,
--    and the moment.
-- 日本の朝6時に貼った1つの依頼：壁の時計、時間帯、瞬間
-- ANCHOR: module
module Zones where

import Data.Time
-- ANCHOR_END: module

-- ANCHOR: zone
-- 🕕 6 a.m. on 13 November 2026, on a wall clock (no place yet).
-- 2026年11月13日の朝6時。壁の時計（まだ、どこの壁かはない）
postedLocal :: LocalTime
postedLocal = LocalTime (fromGregorian 2026 11 13) (TimeOfDay 6 0 0)

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
-- 🌍 The same moment, for the whole world.
-- 同じ瞬間を、世界で1つの形で
postedUtc :: UTCTime
postedUtc = zonedTimeToUTC postedZoned
-- ANCHOR_END: moment
-- ANCHOR_END: all
