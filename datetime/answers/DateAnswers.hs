-- ANCHOR: all
-- 🏋️ Answers to the training of the chapter.
-- 章末のトレーニングの解答
-- ANCHOR: module
module DateAnswers where

import Data.Time
  ( Day
  , LocalTime (..)
  , TimeLocale (..)
  , TimeOfDay (..)
  , TimeZone
  , UTCTime
  , addDays
  , diffDays
  , formatTime
  , localTimeOfDay
  , utcToLocalTime
  )
import Data.Time.Calendar (periodLastDay)
import Data.Time.Calendar.Month (Month)

import Branches (Branch, Reading (..), readWallClock)
import Calendar (payDayClip)
import Clock (Clock)
import Formats (japanese)
-- ANCHOR_END: module

-- ANCHOR: step1
-- 📣 "あと7日（金曜日）" / "今日まで（金曜日）" / "3日過ぎた（月曜日）"
-- 締め切りまでの日数と曜日
dueNotice :: Day -> Day -> String
dueNotice today due =
  let left    = diffDays due today
      weekday = formatTime japanese "%A" due
      howLong
        | left > 0  = "あと" <> show left <> "日"
        | left == 0 = "今日まで"
        | otherwise = show (negate left) <> "日過ぎた"
  in howLong <> "（" <> weekday <> "）"
-- ANCHOR_END: step1

-- ANCHOR: step2
-- 💰 The first n pay days, each counted from the first pay day.
-- 最初の n 回の支払日。どれも最初の支払日から数える
payDays :: Integer -> [Day]
payDays n = [payDayClip k | k <- [0 .. n - 1]]

-- 🌙 The last days of n months, from the given month.
-- その月から n か月分の、月末の日
monthEnds :: Month -> Int -> [Day]
monthEnds month n = map periodLastDay (take n [month ..])
-- ANCHOR_END: step2

-- ANCHOR: step3
-- 🍁 The old Japanese names of the months: 睦月, 如月, ...
-- 和風月名：睦月、如月……
wafu :: TimeLocale
wafu =
  japanese
    { months =
        [ ("睦月", "1月"), ("如月", "2月"), ("弥生", "3月")
        , ("卯月", "4月"), ("皐月", "5月"), ("水無月", "6月")
        , ("文月", "7月"), ("葉月", "8月"), ("長月", "9月")
        , ("神無月", "10月"), ("霜月", "11月"), ("師走", "12月")
        ]
    }

-- 🍂 "霜月13日（金）"
showWafu :: Day -> String
showWafu day = formatTime wafu "%B%-d日（%a）" day
-- ANCHOR_END: step3

-- ANCHOR: step4
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
  | hour >= 5 && hour < 12 = "Good morning"
  | hour >= 12 && hour < 18 = "Good afternoon"
  | otherwise = "Good evening"
-- ANCHOR_END: step4

-- ANCHOR: step5
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
        Once moment    -> [moment]
        Twice first _  -> [first]
        Skipped        -> []
-- ANCHOR_END: step5
-- ANCHOR_END: all
