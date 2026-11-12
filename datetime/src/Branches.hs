-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}
-- 🌐 The branches of the guild, and their named time zones.
-- ギルドの支部と、その名前のある時間帯
-- ANCHOR: module
module Branches
  ( Branch (..)
  , newYorkFixed
  , zoneOf
  , zoneAt
  , wallClockAt
  , Reading (..)
  , readWallClock
  , sameTimeNextDay
  ) where

import Data.Time
  ( LocalTime (..)
  , TimeZone (..)
  , UTCTime
  , ZonedTime
  , addDays
  , utcToZonedTime
  )
import Data.Time.Zones
  ( LocalToUTCResult (..)
  , TZ
  , localTimeToUTCFull
  , timeZoneForUTCTime
  , utcToLocalTimeTZ
  )
import Data.Time.Zones.All (TZLabel (..), tzByLabel)
-- ANCHOR_END: module

-- ANCHOR: branch
-- 🏰 The guild has three branches.
-- ギルドには、支部が3つある
data Branch = Tokyo | NewYork | London
  deriving (Show, Eq, Enum, Bounded)

-- 😵 New York, fixed to summer time (UTC-4) forever.
-- ニューヨークを、夏時間（UTC-4）のまま決め打ちしたもの
newYorkFixed :: TimeZone
newYorkFixed = TimeZone (-4 * 60) True "EDT"
-- ANCHOR_END: branch

-- ANCHOR: zoneOf
-- 🗺️ The named time zone of each branch (from the IANA tz database).
-- 各支部の、名前のある時間帯（IANA の tz データベースから）
zoneOf :: Branch -> TZ
zoneOf Tokyo   = tzByLabel Asia__Tokyo
zoneOf NewYork = tzByLabel America__New_York
zoneOf London  = tzByLabel Europe__London

-- 🔎 The fixed offset of a branch at that moment ("EST", "EDT", ...).
-- その瞬間の、支部の決まったずれ（"EST"、"EDT" など）
zoneAt :: Branch -> UTCTime -> TimeZone
zoneAt branch moment = timeZoneForUTCTime (zoneOf branch) moment

-- 🕰️ The wall clock of a branch at that moment.
-- その瞬間の、支部の壁の時計
wallClockAt :: Branch -> UTCTime -> ZonedTime
wallClockAt branch moment = utcToZonedTime (zoneAt branch moment) moment
-- ANCHOR_END: zoneOf

-- ANCHOR: reading
-- 🔁 A wall clock time can mean one moment, no moment, or two moments.
-- 壁の時計の時刻は、1つの瞬間か、ない瞬間か、2つの瞬間を指す
data Reading
  = Once UTCTime            -- an ordinary time / ふつうの時刻
  | Skipped                 -- the clock jumped over it / 時計が飛ばした
  | Twice UTCTime UTCTime   -- the clock showed it twice / 時計が2回指した
  deriving (Show, Eq)

-- 🧭 Which moment does the wall clock of the branch mean?
-- 支部の壁の時計のこの時刻は、どの瞬間のこと？
readWallClock :: Branch -> LocalTime -> Reading
readWallClock branch wall =
  case localTimeToUTCFull (zoneOf branch) wall of
    LTUUnique moment _            -> Once moment
    LTUNone _ _                   -> Skipped
    LTUAmbiguous first second _ _ -> Twice first second
-- ANCHOR_END: reading

-- ANCHOR: nextDay
-- 📆 The same wall clock time on the next day, in the branch.
--    It is not always 24 hours later.
-- 支部の壁の時計で、次の日の同じ時刻。24 時間後とは限らない
sameTimeNextDay :: Branch -> UTCTime -> Reading
sameTimeNextDay branch moment =
  let wall = utcToLocalTimeTZ (zoneOf branch) moment
      next = wall { localDay = addDays 1 wall.localDay }
  in readWallClock branch next
-- ANCHOR_END: nextDay
-- ANCHOR_END: all
