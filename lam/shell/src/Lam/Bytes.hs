-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | Bytes and the tools for them: bytes from text and back, SHA-256,
--   base64, and the requests that read a file a little at a time.
-- バイト列と、その道具：文字との行き来、SHA-256、base64、それと
-- ファイルを少しずつ読む頼みごと。
module Lam.Bytes
  ( bytesBuiltins
  , decodingOf
  , handleValue
  , notUtf8
  ) where

import qualified Crypto.Hash.SHA256 as Sha
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)

import Lam.Builtins
import Lam.Eval
import Lam.Types
import Lam.Utf8
-- ANCHOR_END: head

-- ANCHOR: table
-- 📋 The rows to add to the table of builtins: Bytes., Hash.,
--    Base64. and File. (each belongs to its module).
-- 組み込みの関数の表に足す行（名前の頭で、それぞれのモジュールのもの）
bytesBuiltins :: [Builtin]
bytesBuiltins =
  textBuiltins <> listBuiltins <> hashBuiltins <> base64Builtins
    <> streamBuiltins
-- ANCHOR_END: table

-- ANCHOR: text
-- 🔤 Text to bytes is always fine (UTF-8). Bytes to text may not be:
--    toString says Err, and decode does what the Decoding says.
-- 文字からバイト列は、いつでもできる（UTF-8）。バイト列から文字は、
-- できないこともある：toString は Err を返し、decode は Decoding の
-- とおりにする
textBuiltins :: [Builtin]
textBuiltins =
  [ prim1 "Bytes.fromString" (tString ~> tBytes) $ \v ->
      VBytes . encodeUtf8 <$> string v
  , prim1 "Bytes.toString" (tBytes ~> tResult tString tString) $ \v -> do
      b <- bytes v
      Right $ case decodeWith Strict b of
        Right t -> con "Ok" [VString t]
        Left at -> con "Err" [VString (notUtf8 at)]
  , prim2 "Bytes.decode" (tDecoding ~> tBytes ~> tString) $ \d v -> do
      how <- decodingOf d
      b   <- bytes v
      case decodeWith how b of
        Right t -> Right (VString t)
        -- Strict stops the program, as Python's b.decode() raises.
        -- Strict はプログラムを止める（Python の b.decode() と同じ）
        Left at -> Left (BuiltinFailed "Bytes.decode" (notUtf8 at))
  ]
-- ANCHOR_END: text

-- ANCHOR: lists
-- 🔢 Bytes as numbers from 0 to 255, and as hex: "e38182".
-- 0 から 255 の数として、16進の文字として
listBuiltins :: [Builtin]
listBuiltins =
  [ prim1 "Bytes.toList" (tBytes ~> tList TInt) $ \v ->
      VList . map (VInt . toInteger) . B.unpack <$> bytes v
  , prim1 "Bytes.fromList" (tList TInt ~> tBytes) $ \v -> do
      ns <- list v >>= traverse byte
      Right (VBytes (B.pack ns))
  , prim1 "Bytes.toHex" (tBytes ~> tString) $ \v ->
      VString . hexOf <$> bytes v
  , prim1 "Bytes.fromHex" (tString ~> tResult tString tBytes) $ \v -> do
      t <- string v
      Right $ case Hex.decode (encodeUtf8 t) of
        Right b -> con "Ok" [VBytes b]
        -- 「16進の文字ではない」
        Left _  -> con "Err" [VString ("not hex: " <> t)]
  , prim3 "Bytes.slice" (TInt ~> TInt ~> tBytes ~> tBytes) $ \f t v -> do
      from <- int f
      to   <- int t
      b    <- bytes v
      Right (VBytes (B.take (to - from) (B.drop from b)))
  , prim1 "Bytes.concat" (tList tBytes ~> tBytes) $ \v ->
      VBytes . B.concat <$> (list v >>= traverse bytes)
  ]
  where
    -- 「バイトは 0 から 255 まで」
    byte n = case n of
      VInt i | i >= 0 && i <= 255 -> Right (fromInteger i)
      VInt i -> Left (BuiltinFailed "Bytes.fromList"
                        ("a byte is from 0 to 255, not " <> T.pack (show i)))
      _      -> wrong "an Int"
-- ANCHOR_END: lists

-- ANCHOR: hash
-- 🔏 SHA-256: at once (Hash.sha256), or a little at a time (start,
--    add, finish), so that a big file never has to be held whole. The
--    Hasher keeps SHA-256's 104 bytes of work inside.
-- SHA-256：一度に（Hash.sha256）か、少しずつ（start・add・finish）。
-- 大きいファイルも、丸ごと持たずにすむ。Hasher の中には、SHA-256 の
-- 途中の 104 バイトが入っている
hashBuiltins :: [Builtin]
hashBuiltins =
  [ prim1 "Hash.sha256" (tBytes ~> tBytes) $ \v ->
      VBytes . Sha.hash <$> bytes v
  , Builtin "Hash.start" (closeOver tHasher) (hasher Sha.init)
  , prim2 "Hash.add" (tBytes ~> tHasher ~> tHasher) $ \v h -> do
      b   <- bytes v
      ctx <- hasherOf h
      Right (hasher (Sha.update ctx b))
  , prim1 "Hash.finish" (tHasher ~> tBytes) $ \h ->
      VBytes . Sha.finalize <$> hasherOf h
  ]
  where
    hasher (Sha.Ctx b) = con "Hasher" [VBytes b]
    hasherOf v = case v of
      VCon "Hasher" _ [VBytes b] -> Right (Sha.Ctx b)
      _ -> wrong "a Hasher"
-- ANCHOR_END: hash

-- ANCHOR: base64
-- 📮 base64: bytes as 64 kinds of letters, for places that take only
--    text (JSON, e-mail, data: URLs).
-- base64：バイト列を64種類の文字で。文字しか通らない所（JSON、
-- メール、data: の URL）のために
base64Builtins :: [Builtin]
base64Builtins =
  [ prim1 "Base64.encode" (tBytes ~> tString) $ \v ->
      VString . decodeLatin1 . Base64.encode <$> bytes v
  , prim1 "Base64.decode" (tString ~> tResult tString tBytes) $ \v -> do
      t <- string v
      Right $ case Base64.decode (encodeUtf8 t) of
        Right b  -> con "Ok" [VBytes b]
        -- 「base64 ではない」
        Left why -> con "Err" [VString ("not base64: " <> T.pack why)]
  ]
-- ANCHOR_END: base64

-- ANCHOR: stream
-- 📂 The requests of a file read a little at a time, and of a file
--    as bytes. File.nextLine and File.nextChunk give an Err for a
--    broken line or a closed handle; the File module (in Lam) turns it
--    into a stop.
-- 少しずつ読むファイルと、バイト列のファイルの頼みごと。壊れた行や
-- 閉じたハンドルに、File.nextLine と File.nextChunk は Err を返す。
-- モジュール File（Lam で書く）が、それを止まることに変える
streamBuiltins :: [Builtin]
streamBuiltins =
  [ prim2 "File.openWith"
      (tDecoding ~> tString ~> tIO (tResult tString tHandle)) $ \d p ->
      VIO . Ask <$> (OpenFile <$> string p <*> decodingOf d)
  , prim1 "File.nextLine"
      (tHandle ~> tIO (tResult tString (tMaybe tString))) $ \h ->
      VIO . Ask . ReadLine <$> handleOf h
  , prim2 "File.nextChunk"
      (TInt ~> tHandle ~> tIO (tResult tString (tMaybe tBytes))) $ \n h ->
      VIO . Ask <$> (ReadChunk <$> handleOf h <*> int n)
  , prim1 "File.close" (tHandle ~> tIO tUnit) $ \h ->
      VIO . Ask . CloseFile <$> handleOf h
  , prim1 "File.readBytes" (tString ~> tIO (tResult tString tBytes)) $ \p ->
      VIO . Ask . ReadBytes <$> string p
  , prim2 "File.writeBytes"
      (tString ~> tBytes ~> tIO (tResult tString tUnit)) $ \p v ->
      VIO . Ask <$> (WriteBytes <$> string p <*> bytes v)
  ]
-- ANCHOR_END: stream

-- ANCHOR: values
-- 🧱 A handle is a hidden constructor around the world's number.
-- ハンドルは、世界がくれた番号を、隠した構成子で包んだもの
handleValue :: Int -> Value
handleValue n = con "Handle" [VInt (toInteger n)]

handleOf :: Value -> Either EvalError Int
handleOf v = case v of
  VCon "Handle" _ [VInt n] -> Right (fromInteger n)
  _ -> wrong "a Handle"

-- 🧩 Strict, Replace, Ignore of Lam, as the Haskell ones.
-- Lam の Strict・Replace・Ignore を、Haskell のものに
decodingOf :: Value -> Either EvalError Decoding
decodingOf v = case v of
  VCon "Strict" _ []  -> Right Strict
  VCon "Replace" _ [] -> Right Replace
  VCon "Ignore" _ []  -> Right Ignore
  _ -> wrong "a Decoding"

-- 「byte 4 から UTF-8 ではない」
notUtf8 :: Int -> Text
notUtf8 at = "not UTF-8 from byte " <> T.pack (show at)

bytes :: Value -> Either EvalError B.ByteString
bytes v = case v of
  VBytes b -> Right b
  _        -> wrong "Bytes"

int :: Value -> Either EvalError Int
int v = case v of
  VInt n -> Right (fromInteger n)
  _      -> wrong "an Int"

tBytes, tDecoding, tHandle, tHasher :: Type
tBytes    = TCon "Bytes" []
tDecoding = TCon "Decoding" []
tHandle   = TCon "Handle" []
tHasher   = TCon "Hasher" []

tResult :: Type -> Type -> Type
tResult e x = TCon "Result" [e, x]
-- ANCHOR_END: values
-- ANCHOR_END: all
