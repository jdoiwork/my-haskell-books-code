-- ANCHOR: all
{-# LANGUAGE TemplateHaskell #-}
-- | The files the pages need (htmx, the shapes, and the colours), put
--   inside the program when it is compiled. The shelf is then one file
--   to carry.
-- ページが使うファイル（htmx、形、色）。コンパイルのときにプログラムの
-- 中に入れる。本棚は、ファイル1つで持ち運べる
module Tsundoku.Html.Static (staticFiles) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir)

-- 📦 Each file under static/, as (path, bytes).
-- static/ の下のファイルの1つ1つを、（パス、バイト列）で
staticFiles :: [(FilePath, ByteString)]
staticFiles = $(embedDir "static")
-- ANCHOR_END: all
