-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | The answers of the training that are written in Haskell.
-- トレーニングの解答のうち、Haskell で書くもの。
module Lam.Answers
  ( gunzipBuiltin
  ) where

import qualified Codec.Compression.Zlib.Internal as Z
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Lam.Builtins
import Lam.Eval
import Lam.Types

-- ANCHOR: gunzip
-- 🗜️ Step 4: Bytes.gunzip, the bytes of a .gz file made whole again.
--    A broken .gz is an Err (zlib says why), not a crash.
-- ステップ4：.gz のファイルのバイト列を、元に戻す。壊れた .gz は
-- Err（zlib がわけを言う）で、止まらない
gunzipBuiltin :: Builtin
gunzipBuiltin =
  prim1 "Bytes.gunzip" (tBytes ~> tResult tString tBytes) $ \v -> case v of
    VBytes packed -> Right $
      case Z.foldDecompressStreamWithInput
             (\piece rest -> (piece :) <$> rest)   -- a piece came out
             (\_ -> Right [])                       -- the end
             Left                                   -- broken
             (Z.decompressST Z.gzipFormat Z.defaultDecompressParams)
             (BL.fromStrict packed) of
        Right pieces -> con "Ok" [VBytes (B.concat pieces)]
        -- 「gzip ではない」
        Left err -> con "Err" [VString ("not gzip: " <> T.pack (show err))]
    _ -> wrong "Bytes"
  where
    tBytes = TCon "Bytes" []
    tResult e x = TCon "Result" [e, x]
-- ANCHOR_END: gunzip
-- ANCHOR_END: all
