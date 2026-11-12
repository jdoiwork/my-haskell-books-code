-- ANCHOR: all
-- ⏰ Clocks: the real one, and stopped ones for the tests.
-- 時計：本物の時計と、テストのための止まった時計
-- ANCHOR: module
module Clock
  ( Clock
  , systemClock
  , stoppedAt
  , postQuest
  ) where

import Data.Time (TimeZone, UTCTime, getCurrentTime)

import Board (Quest, postIn)
-- ANCHOR_END: module

-- ANCHOR: clock
-- ⏰ A clock is just an action that tells the time.
-- 時計は、時刻を教えてくれる動作にすぎない
type Clock = IO UTCTime

-- 🌍 The real clock of the computer.
-- パソコンの本物の時計
systemClock :: Clock
systemClock = getCurrentTime

-- 🧊 A stopped clock: it always says the same time.
-- 止まった時計：いつも同じ時刻を言う
stoppedAt :: UTCTime -> Clock
stoppedAt moment = pure moment
-- ANCHOR_END: clock

-- ANCHOR: post
-- 📌 Post a quest now. The clock and the time zone come from outside.
-- いま依頼を貼る。時計と時間帯は、外から渡してもらう
postQuest :: Clock -> TimeZone -> String -> IO Quest
postQuest clock zone name = do
  now <- clock
  pure (postIn zone name now)
-- ANCHOR_END: post
-- ANCHOR_END: all
