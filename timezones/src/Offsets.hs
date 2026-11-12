-- ANCHOR: all
-- 🤝 Moments written with "Z", or with an offset such as "+09:00".
-- "Z" で書いた瞬間と、"+09:00" のようなずれつきで書いた瞬間
-- ANCHOR: module
module Offsets where

import Data.Time (UTCTime, zonedTimeToUTC)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
-- ANCHOR_END: module

-- ANCHOR: iso
-- 🌐 Write a moment for the borders, always in UTC with "Z".
-- 境目に出す瞬間は、いつも UTC の "Z" で書く
toWire :: UTCTime -> String
toWire moment = iso8601Show moment

-- 🔍 Read only the "Z" form.
-- "Z" の形だけを読む
fromWire :: String -> Maybe UTCTime
fromWire text = iso8601ParseM text
-- ANCHOR_END: iso

-- ANCHOR: isoAny
-- 🤝 Read a moment written with "Z" or with an offset ("+09:00").
-- "Z" でも、ずれ（"+09:00"）つきでも読む
fromWireAny :: String -> Maybe UTCTime
fromWireAny text =
  case iso8601ParseM text of
    Just moment -> Just moment
    Nothing     -> fmap zonedTimeToUTC (iso8601ParseM text)
-- ANCHOR_END: isoAny
-- ANCHOR_END: all
