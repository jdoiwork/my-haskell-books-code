-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}

-- ANCHOR: module
module Peek where

-- JSON: decoding to a Value, and running a parser by hand.
-- JSON：Value への読み込みと、パーサーを手で動かす道具
import Data.Aeson       (decode, withObject, (.:))
import Data.Aeson.Types (parseMaybe)

-- Bytes.
-- バイト列
import Data.ByteString.Lazy (ByteString)
-- ANCHOR_END: module

-- ANCHOR: peek
-- 🔎 Take only the "version" out of a save file.
--    We do not care about the other fields.
-- セーブデータから "version" だけを取り出す
-- ほかのフィールドは気にしない
saveVersion :: ByteString -> Maybe Int
saveVersion bytes =
  case decode bytes of
    Nothing    -> Nothing
    Just value -> parseMaybe (withObject "save" (\o -> o .: "version")) value
-- ANCHOR_END: peek

-- ANCHOR: peekNested
-- 🔎 Go down two levels: {"hero": {"level": 12}}.
-- 2段下りる：{"hero": {"level": 12}}
heroLevel :: ByteString -> Maybe Int
heroLevel bytes =
  case decode bytes of
    Nothing    -> Nothing
    Just value -> parseMaybe findLevel value
  where
    findLevel = withObject "save" $ \o -> do
      hero <- o .: "hero"
      hero .: "level"
-- ANCHOR_END: peekNested
-- ANCHOR_END: all
