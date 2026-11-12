-- ANCHOR: all
-- 🧮 Counting days, weeks, and months for the quests.
-- 依頼の日・週・月を数える
-- ANCHOR: module
module Calendar where

import Data.Time
  ( Day
  , DayOfWeek (..)
  , addDays
  , addGregorianMonthsClip
  , addGregorianMonthsRollOver
  , dayOfWeek
  , diffDays
  , fromGregorian
  )
import Data.Time.Calendar.Month (Month)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Calendar
  ( dayPeriod
  , periodFirstDay
  , periodLastDay
  )
-- ANCHOR_END: module

-- ANCHOR: days
-- ⏳ How many days are left until the due day?
-- 締め切りの日まで、あと何日？
daysLeft :: Day -> Day -> Integer
daysLeft today due = diffDays due today

-- 🛌 The board is closed on weekends: move the day to Monday.
-- 週末は掲示板がお休み。月曜日にずらす
onBoardDay :: Day -> Day
onBoardDay day =
  case dayOfWeek day of
    Saturday -> addDays 2 day
    Sunday   -> addDays 1 day
    _        -> day
-- ANCHOR_END: days

-- ANCHOR: months
-- 🗓️ The guild pays its members on the 31st of January, then monthly.
-- ギルドは 1月31日に、それから毎月、報酬を払う
firstPayDay :: Day
firstPayDay = fromGregorian 2027 1 31

-- ✂️ n months later. A day that the month does not have is cut down
--    to the month's last day.
-- n か月後。その月にない日は、月末に切り詰める
payDayClip :: Integer -> Day
payDayClip n = addGregorianMonthsClip n firstPayDay

-- 🛞 n months later. A day that the month does not have rolls over
--    into the next month.
-- n か月後。その月にない日は、次の月へはみ出す
payDayRollOver :: Integer -> Day
payDayRollOver n = addGregorianMonthsRollOver n firstPayDay

-- 😵 Add one month again and again. The day drifts to the 28th.
-- 1か月ずつ、くり返し足す。日が 28 日にずれていく
payDaysDrift :: [Day]
payDaysDrift = iterate (addGregorianMonthsClip 1) firstPayDay
-- ANCHOR_END: months

-- ANCHOR: periods
-- 📆 The ISO week: (year, week number, day of week from 1 = Monday).
-- ISO の週：（年、週の番号、曜日。月曜日が 1）
weekOf :: Day -> (Integer, Int, Int)
weekOf day = toWeekDate day

-- 🌙 The month a day is in, and its first and last days.
-- その日の月と、月の最初の日・最後の日
monthOf :: Day -> (Month, Day, Day)
monthOf day =
  let month = dayPeriod day
  in (month, periodFirstDay month, periodLastDay month)
-- ANCHOR_END: periods
-- ANCHOR_END: all
