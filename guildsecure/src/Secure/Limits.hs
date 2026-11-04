-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}

-- 🧱 Things types do not stop: huge inputs and huge numbers.
-- 型では止まらないもの：巨大な入力と、巨大な数
module Secure.Limits
  ( decodeLimited
  , goldFromJson
  ) where

import Data.Aeson                 (FromJSON, Value (..), eitherDecode)
import Data.ByteString.Lazy       qualified as BL
import Data.Int                   (Int64)
import Data.Scientific            (toBoundedInteger)

-- ANCHOR: limited
-- 🧱 Refuse a body that is too large, before even reading it as JSON.
--    compareLength counts only up to maxBytes + 1, so it stops early
--    even when the body is endless (BL.length would count to the end).
-- 大きすぎる本体は、JSON として読む前に断る。
-- compareLength は maxBytes + 1 まで数えたら止まるので、本体が終わらなくても
-- すぐ答える（BL.length は最後まで数えてしまう）
decodeLimited :: FromJSON a => Int64 -> BL.ByteString -> Either String a
decodeLimited maxBytes body
  | BL.compareLength body maxBytes == GT = Left "the body is too large"
  | otherwise                            = eitherDecode body
-- ANCHOR_END: limited

-- ANCHOR: gold
-- 🪙 A gold amount from a JSON number, only if it fits in an Int.
--    1e1000000000 does not fit: Nothing (instead of a huge Integer).
-- JSON の数から金額を取り出す。Int に収まるときだけ。
-- 1e1000000000 は収まらないので Nothing（巨大な Integer を作らない）
goldFromJson :: Value -> Maybe Int
goldFromJson (Number n) = toBoundedInteger n
goldFromJson _          = Nothing
-- ANCHOR_END: gold
-- ANCHOR_END: all
