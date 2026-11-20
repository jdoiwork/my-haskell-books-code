-- ANCHOR: all
{-# LANGUAGE QuasiQuotes #-}
-- A pattern with a mistake: the build stops here, not when it runs.
-- まちがいのあるパターン。動かしたときではなく、ここでビルドが止まる
module BadPattern where

import MiniRegex
import MiniRegex.Quote (re)

-- 📅 A year and a month, with the ( of the month left open.
-- 年と月。月の ( を閉じ忘れた
datePattern :: Regex
datePattern = [re|(?<year>\d{4})-(?<month>\d{2}|]
-- ANCHOR_END: all
