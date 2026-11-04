-- ANCHOR: all
-- 📅 Days and deadlines of quests.
-- 依頼の日付と締め切り
module Deadline where

import Data.Time
  ( Day
  , addDays
  , defaultTimeLocale
  , diffDays
  , formatTime
  , fromGregorian
  , parseTimeM
  )

-- ANCHOR: days
-- 📌 The day a quest was posted on the board.
-- 依頼が貼り出された日
posted :: Day
posted = fromGregorian 2026 11 1

-- ⏰ Every quest must be done within 7 days.
-- どの依頼も、7日以内に終わらせる
deadline :: Day -> Day
deadline day = addDays 7 day

-- ⏳ How many days are left until the deadline?
-- 締め切りまで、あと何日？
daysLeft :: Day -> Day -> Integer
daysLeft today postedDay = diffDays (deadline postedDay) today
-- ANCHOR_END: days

-- ANCHOR: format
-- 🖨️ "2026-11-08 (Sun)"
showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%Y-%m-%d (%a)"

-- 🔍 Read "2026-11-08" as a Day. A wrong date is Nothing.
-- "2026-11-08" を Day として読む。おかしな日付なら Nothing
parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"
-- ANCHOR_END: format
-- ANCHOR_END: all
