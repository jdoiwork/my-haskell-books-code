-- ANCHOR: all
-- 🏋️ Answers to the training of the chapter.
-- 章末のトレーニングの解答
-- ANCHOR: module
module DateAnswers where

import Data.Time
  ( Day
  , NominalDiffTime
  , TimeLocale (..)
  , diffDays
  , formatTime
  )
import Data.Time.Calendar (periodLastDay)
import Data.Time.Calendar.Month (Month)

import Calendar (payDayClip)
import Formats (japanese)
-- ANCHOR_END: module

-- ANCHOR: step1
-- 📣 "あと7日（金曜日）" / "今日まで（金曜日）" / "3日過ぎた（火曜日）"
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
-- ⏳ "あと1日2時間5分". A part of a minute counts as a whole minute.
-- 残り時間を日・時間・分で。1分に満たない端数は、1分と数える
remaining :: NominalDiffTime -> String
remaining left
  | left <= 0 = "締め切りを過ぎた"
  | otherwise =
      let minutes       = ceiling (left / 60) :: Integer
          (days, rest)  = minutes `divMod` (24 * 60)
          (hours, mins) = rest `divMod` 60
      in "あと" <> show days <> "日" <> show hours <> "時間"
           <> show mins <> "分"
-- ANCHOR_END: step4
-- ANCHOR_END: all
