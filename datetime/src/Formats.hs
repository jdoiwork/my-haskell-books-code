-- ANCHOR: all
-- 🖨️ Writing and reading dates and times.
-- 日付と時刻を書く・読む
-- ANCHOR: module
module Formats where

import Data.Time
  ( Day
  , LocalTime
  , TimeLocale (..)
  , UTCTime
  , addDays
  , defaultTimeLocale
  , formatTime
  , parseTimeM
  )
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
-- ANCHOR_END: module

-- ANCHOR: iso
-- 🌐 The format for the borders (files, JSON, databases): ISO 8601.
--    "2026-11-13T09:00:00Z"
-- 境目（ファイル、JSON、データベース）の書式：ISO 8601
toWire :: UTCTime -> String
toWire moment = iso8601Show moment

-- 🔍 Read it back. A broken string is Nothing.
-- 読み戻す。壊れた文字なら Nothing
fromWire :: String -> Maybe UTCTime
fromWire text = iso8601ParseM text
-- ANCHOR_END: iso

-- ANCHOR: locale
-- 🎌 Japanese names for the days of the week, and 午前 / 午後.
-- 日本語の曜日の名前と、午前・午後
japanese :: TimeLocale
japanese =
  defaultTimeLocale
    { wDays =
        [ ("日曜日", "日")
        , ("月曜日", "月")
        , ("火曜日", "火")
        , ("水曜日", "水")
        , ("木曜日", "木")
        , ("金曜日", "金")
        , ("土曜日", "土")
        ]
    , amPm  = ("午前", "午後")
    }

-- 🪧 "2026年11月13日（金）午前9時00分"
-- 掲示板に貼る、日本語の日時
showJapanese :: LocalTime -> String
showJapanese moment =
  formatTime japanese "%Y年%-m月%-d日（%a）%p%-I時%M分" moment
-- ANCHOR_END: locale

-- ANCHOR: parse
-- 🇺🇸 A day written by the New York branch: month/day/year.
-- ニューヨーク支部の書き方：月/日/年
fromNewYork :: String -> Maybe Day
fromNewYork text = parseTimeM False defaultTimeLocale "%m/%d/%Y" text

-- 🇬🇧 A day written by the London branch: day/month/year.
-- ロンドン支部の書き方：日/月/年
fromLondon :: String -> Maybe Day
fromLondon text = parseTimeM False defaultTimeLocale "%d/%m/%Y" text
-- ANCHOR_END: parse

-- ANCHOR: gate
-- 🚧 A due day must be read, and also make sense:
--    from today to one year later.
-- 締め切りの日は、読めるだけでなく、意味が通ること：今日から1年後まで
dueDayFrom :: Day -> String -> Either String Day
dueDayFrom today text =
  case iso8601ParseM text of
    Nothing -> Left ("not a day: " <> show text)
    Just due
      | due < today             -> Left ("already past: " <> show due)
      | due > addDays 365 today -> Left ("too far away: " <> show due)
      | otherwise               -> Right due
-- ANCHOR_END: gate
-- ANCHOR_END: all
