-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- 🔤 Text (characters) and ByteString (bytes).
-- Text（文字）と ByteString（バイト）
module Bytes where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
-- ANCHOR_END: imports

-- ANCHOR: encode
-- 📦 A name, as the bytes that are sent over the network or saved.
-- 名前を、通信や保存に使うバイト列にする
nameBytes :: Text -> ByteString
nameBytes = encodeUtf8

-- 🔢 How many characters, and how many bytes?
-- 文字はいくつ、バイトはいくつ？
sizes :: Text -> (Int, Int)
sizes t = (T.length t, BS.length (encodeUtf8 t))
-- ANCHOR_END: encode

-- ANCHOR: decode
-- 🔍 Bytes from outside may not be valid UTF-8, so decode safely.
-- 外から来たバイト列は、正しい UTF-8 とは限らないので、安全に読む
safeDecode :: ByteString -> Either Text Text
safeDecode bytes =
  case decodeUtf8' bytes of
    Left err -> Left (T.pack (show err))
    Right t  -> Right t
-- ANCHOR_END: decode
-- ANCHOR_END: all
