-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | Times as text: the ISO 8601 text of a time, and the offset of a
--   zone. The evaluator shows a Time with them, and Lam.Files reads
--   and writes with them.
-- 時刻を文字に：時刻の ISO 8601 の文字と、ゾーンの時差。評価器は
-- これで Time を見せ、Lam.Files は、これで読み書きする。
module Lam.Clock
  ( isoOf
  , offsetText
  , utcOf
  , millisOf
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

-- 🕰️ The ISO 8601 text of a time, in UTC, with the milliseconds only
--    when there are some:  2026-11-17T03:00:00Z
-- 時刻の ISO 8601 の文字。UTC で。ミリ秒は、あるときだけ
isoOf :: Integer -> Text
isoOf ms =
  let whole = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (utcOf ms)
      rest  = ms `mod` 1000
  in T.pack whole
       <> (if rest == 0 then ""
           else "." <> T.justifyRight 3 '0' (T.pack (show rest)))
       <> "Z"

-- 🌐 The offset of a zone, from its minutes ahead of UTC:  540  is
--    +09:00, and  -300  is  -05:00.
-- ゾーンの時差を、UTC より進んだ分（分）から
offsetText :: Int -> Text
offsetText m =
  let sign    = if m < 0 then "-" else "+"
      (h, mm) = abs m `divMod` 60
  in sign <> two h <> ":" <> two mm
  where
    two n = T.justifyRight 2 '0' (T.pack (show n))

-- ⏱️ The milliseconds since 1970 and Haskell's UTCTime, both ways
--    (finer than a millisecond is cut down).
-- 1970年からのミリ秒と、Haskell の UTCTime を行き来する（ミリ秒より
-- 細かいところは切り捨て）
utcOf :: Integer -> UTCTime
utcOf ms = posixSecondsToUTCTime (fromInteger ms / 1000)

millisOf :: UTCTime -> Integer
millisOf t = floor (utcTimeToPOSIXSeconds t * 1000)
-- ANCHOR_END: all
