{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- ANCHOR: head
-- | The builtins that read CSV and TOML. Each one only turns the text
--   into Lam's values: the rows of CSV into lists of strings, a TOML
--   document into Json. The modules Csv, Toml and Decode (written in
--   Lam, in Lam.Prelude) do the rest.
-- CSV と TOML を読む組み込み。どちらも、文字を Lam の値にするだけ。
-- CSV の行は文字列のリストに、TOML は Json に。残りは、Lam で書いた
-- モジュール Csv・Toml・Decode（Lam.Prelude）がする。
module Lam.Decode
  ( decodeBuiltins
  , csvRows
  , tomlJson
  ) where

-- The libraries that read: cassava (CSV) and toml-parser (TOML)
-- 読むライブラリ：cassava（CSV）と toml-parser（TOML）
import qualified Data.Csv as Csv
import qualified Toml

-- Text, bytes, the clock (TOML has dates), and the containers
-- 文字、バイト列、時計（TOML には日付がある）、入れ物
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.Vector as V

import Lam.Builtins
import Lam.Eval
import Lam.Json (fromScientific)
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: table
-- 📋 The rows to add to the table of builtins.
-- 組み込みの関数の表に足す行
decodeBuiltins :: [Builtin]
decodeBuiltins =
  [ prim1 "Csv.rows" (tString ~> tResult tString (tList (tList tString)))
      $ \v -> do
        text <- string v
        Right $ case csvRows text of
          Right rows -> con "Ok"
            [VList [ VList (map VString row) | row <- rows ]]
          Left msg -> con "Err" [VString (T.pack msg)]
  , prim1 "Toml.parse" (tString ~> tResult tString tJson) $ \v -> do
      text <- string v
      Right $ case tomlJson text of
        Right j  -> con "Ok" [j]
        Left msg -> con "Err" [VString (T.pack msg)]
  ]
  where
    tJson = TCon "Json" []
    tResult e a = TCon "Result" [e, a]
-- ANCHOR_END: table

-- ANCHOR: csvRows
-- 📄 The rows of CSV, every field a string. cassava knows the rules
--    of RFC 4180: quotes, "" inside them, commas and line breaks in a
--    field, and \r\n at the ends of the lines. It does not look at the
--    first row: the module Csv makes it the header.
-- CSV の行。フィールドはどれも文字列。cassava は RFC 4180 の決まり
-- （引用符、中の ""、フィールドの中のカンマと改行、行の終わりの \r\n）
-- を知っている。1行目は見ない。見出しにするのは、モジュール Csv
csvRows :: Text -> Either String [[Text]]
csvRows text =
  V.toList <$> Csv.decode Csv.NoHeader (BL.fromStrict (TE.encodeUtf8 text))
-- ANCHOR_END: csvRows

-- ANCHOR: tomlJson
-- 🧾 A TOML document, as Lam's Json. A table is an object, an array is
--    an array. TOML has more kinds of values than JSON: a float is the
--    shortest number of JSON (NaN and the infinities are null, as
--    Json.fromFloat), and a date or a time is a string of ISO 8601.
-- TOML の文書を、Lam の Json にする。表はオブジェクト、配列は配列。
-- TOML は JSON より値の種類が多い。小数はいちばん短い JSON の数
-- （NaN と無限大は Json.fromFloat と同じく null）、日付や時刻は
-- ISO 8601 の文字列にする
tomlJson :: Text -> Either String Value
tomlJson text = table . Toml.forgetTableAnns <$> Toml.parse text
  where
    table (Toml.MkTable m) = con "JObj" [VMap (Map.fromList
      [ (Key (VString k), value x) | (k, (_, x)) <- Map.toList m ])]
    value x = case x of
      Toml.Integer n   -> con "JInt" [VInt n]
      Toml.Double d
        | isNaN d || isInfinite d -> con "JNull" []
        | otherwise               -> fromScientific (fromFloatDigits d)
      Toml.Bool b      -> con "JBool" [VBool b]
      Toml.Text s      -> str s
      Toml.List xs     -> con "JArr" [VList (map value xs)]
      Toml.Table t     -> table t
      Toml.Day d       -> str (T.pack (iso8601Show d))
      Toml.LocalTime t -> str (T.pack (iso8601Show t))
      Toml.ZonedTime t -> str (T.pack (iso8601Show t))
      Toml.TimeOfDay t -> str (T.pack (iso8601Show t))
    str s = con "JStr" [VString s]
-- ANCHOR_END: tomlJson
-- ANCHOR_END: all
