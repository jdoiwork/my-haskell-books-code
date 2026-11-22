-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | Bytes and text: UTF-8 with three ways to treat a broken part, and
--   how the REPL shows bytes. The evaluator, the builtins and both
--   worlds use them.
-- バイト列と文字：壊れた所の扱いが3つある UTF-8 と、REPL での
-- バイト列の見せ方。評価器、組み込みの関数、2つの世界が使う。
module Lam.Utf8
  ( Decoding (..)
  , decodeWith
  , brokenAt
  , renderBytes
  , hexOf
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Hex
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient, decodeUtf8With,
                           decodeLatin1, validateUtf8Chunk)

-- ANCHOR: decoding
-- 🧩 What to do with bytes that are not UTF-8 (Python's errors=
--    "strict", "replace" and "ignore").
-- UTF-8 でないバイトをどうするか（Python の errors= の3つと同じ）
data Decoding
  = Strict    -- stop, and say where       止まって、場所を言う
  | Replace   -- put U+FFFD (�) there      そこに U+FFFD（�）を置く
  | Ignore    -- leave them out            とばす
  deriving (Eq, Show)
-- ANCHOR_END: decoding

-- ANCHOR: decodeWith
-- 📖 Bytes to text. Only Strict can fail: Left is the number of the
--    first byte that is not UTF-8 (from 0).
-- バイト列を文字に。失敗するのは Strict だけ：Left は、UTF-8 でない
-- 最初のバイトの番号（0 から）
decodeWith :: Decoding -> ByteString -> Either Int Text
decodeWith how bytes = case how of
  Strict  -> maybe (Right (decodeUtf8Lenient bytes)) Left (brokenAt bytes)
  Replace -> Right (decodeUtf8Lenient bytes)
  Ignore  -> Right (decodeUtf8With (\_ _ -> Nothing) bytes)

-- 🔍 Where the bytes stop being UTF-8, or Nothing if they never do.
--    A character cut at the end is broken too, where it starts.
-- どこから UTF-8 でなくなるか。ずっと UTF-8 なら Nothing。最後で
-- 切れた文字も、その文字の頭から壊れている
brokenAt :: ByteString -> Maybe Int
brokenAt bytes = case validateUtf8Chunk bytes of
  (n, Just _) | n == B.length bytes -> Nothing   -- all good   全部よい
  (n, _)                            -> Just n
-- ANCHOR_END: decodeWith

-- ANCHOR: renderBytes
-- 🖨️ <3 bytes: e38182>. A long one shows its first 16 bytes and …
-- 長いものは、頭の16バイトと … だけ
renderBytes :: ByteString -> Text
renderBytes bytes =
  "<" <> T.pack (show n) <> (if n == 1 then " byte" else " bytes")
    <> (if n == 0 then "" else ": " <> hexOf (B.take 16 bytes)
         <> (if n > 16 then "…" else ""))
    <> ">"
  where
    n = B.length bytes

-- 🔢 The bytes in hex: e38182 (two small letters for each byte).
-- 16進で（1バイトに小文字2つ）
hexOf :: ByteString -> Text
hexOf = decodeLatin1 . Hex.encode
-- ANCHOR_END: renderBytes
-- ANCHOR_END: all
