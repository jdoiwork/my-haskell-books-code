-- ANCHOR: all
-- 🪣 Handing a moment over to the others: Unix seconds and
--    JavaScript milliseconds.
-- 日時をほかの人へ渡す：Unix の秒と、JavaScript のミリ秒
-- ANCHOR: module
module Wire where

import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
  ( POSIXTime
  , posixSecondsToUTCTime
  , utcTimeToPOSIXSeconds
  )
-- ANCHOR_END: module

-- ANCHOR: posix
-- ⏲️ Seconds since 1970-01-01 00:00 UTC (they may have a fraction).
-- 1970-01-01 00:00 UTC からの秒数（小数もある）
toUnixSeconds :: UTCTime -> POSIXTime
toUnixSeconds moment = utcTimeToPOSIXSeconds moment

-- 🟨 Milliseconds, as JavaScript's Date keeps them.
--    The part below a millisecond is cut off.
-- JavaScript の Date が持つミリ秒。ミリ秒より細かい所は切り捨てる
toJsMillis :: UTCTime -> Integer
toJsMillis moment = floor (utcTimeToPOSIXSeconds moment * 1000)

-- 🟨 Back from JavaScript's milliseconds.
-- JavaScript のミリ秒から戻す
fromJsMillis :: Integer -> UTCTime
fromJsMillis millis = posixSecondsToUTCTime (fromInteger millis / 1000)
-- ANCHOR_END: posix
-- ANCHOR_END: all
