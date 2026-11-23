{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- ANCHOR: head
-- | The builtins of "Making the tools sturdier": the String names that
--   were missing (the same builtins under a second name), and the rows
--   of CSV with the line each field starts on.
-- 「道具を補強する」の組み込み：抜けていた String の名前（同じ組み込みに
-- もう1つの名前）と、フィールドごとに始まる行の付いた CSV の行。
module Lam.Toolbox
  ( toolboxBuiltins
  , csvLines
  , numberedRows
  , tomlLine
  ) where

import Data.Char (isDigit)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Builtins
import Lam.Decode (csvRows)
import Lam.Eval
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: table
-- 📋 The rows to add to the table of builtins.
-- 組み込みの関数の表に足す行
toolboxBuiltins :: [Builtin]
toolboxBuiltins = stringNames <> [numberedRowsBuiltin, tomlPlaceBuiltin]
-- ANCHOR_END: table

-- ANCHOR: stringNames
-- 🔤 String.toLower, String.join, ...: the builtins the prelude has
--    already (toLower, join, ...), under a second name. They are the
--    same values, so they do the same work.
-- String.toLower・String.join など：プレリュードにもうある組み込み
-- （toLower・join など）に、もう1つの名前を付ける。同じ値なので、
-- することも同じ
stringNames :: [Builtin]
stringNames =
  [ b { builtinName = "String." <> builtinName b }
  | b <- builtins
  , builtinName b `elem` ["toLower", "toUpper", "join", "lines", "words"] ]
-- ANCHOR_END: stringNames

-- ANCHOR: numberedRows
-- 📄 Csv.numberedRows: the rows of CSV, each field with the line of the
--    file it starts on (from 1). cassava reads the fields; csvLines
--    finds the lines, by the same rules.
-- Csv.numberedRows：CSV の行。どのフィールドにも、それが始まるファイルの
-- 行（1 から）を付ける。フィールドを読むのは cassava、行を探すのは
-- csvLines（同じ決まりで）
numberedRowsBuiltin :: Builtin
numberedRowsBuiltin =
  prim1 "Csv.numberedRows"
    (tString ~> tResult tString (tList (tList (tTuple [TInt, tString]))))
    $ \v -> do
      text <- string v
      Right $ case numberedRows text of
        Right rows -> con "Ok" [VList (map row rows)]
        Left msg   -> con "Err" [VString (T.pack msg)]
  where
    tResult e a = TCon "Result" [e, a]
    row cells = VList [ VTuple [VInt (toInteger n), VString s]
                      | (n, s) <- cells ]

-- 📄 The fields of cassava, with the lines of csvLines. The two agree
--    on a CSV that cassava reads; if they did not, a field takes the
--    line of the field before it.
-- cassava のフィールドに、csvLines の行を付ける。cassava が読める CSV
-- なら、2つは合う。合わなければ、前のフィールドの行を使う
numberedRows :: Text -> Either String [[(Int, Text)]]
numberedRows text = do
  rows <- csvRows text
  pure (go 1 rows (csvLines text))
  where
    go _ [] _ = []
    go before (cells : rest) found = case found of
      ls : more -> withLines before cells ls : go (lastOf before ls) rest more
      []        -> withLines before cells [] : go before rest []
    withLines before cells ls =
      zip (fill before ls (length cells)) cells
    fill before ls n = take n (ls <> repeat (lastOf before ls))
    lastOf before ls = if null ls then before else last ls
-- ANCHOR_END: numberedRows

-- ANCHOR: csvLines
-- 🔢 For each row of CSV, the line each field starts on. The rules are
--    RFC 4180's, as cassava reads them: a field in quotes may hold
--    commas, "" and line breaks; a line ends with \n or \r\n; and an
--    empty line is not a row.
-- CSV の行ごとに、フィールドが始まる行。決まりは RFC 4180 のもので、
-- cassava と同じ読み方：引用符の中のフィールドには、カンマ・""・改行が
-- 入ってよい。行は \n か \r\n で終わる。空の行は、行に数えない
csvLines :: Text -> [[Int]]
csvLines = rows 1 . T.unpack
  where
    -- At the start of a line: an empty line is skipped, and so is a
    -- row of one empty field ("" alone), as cassava does.
    -- 行の頭：空の行は飛ばす。空のフィールド1つだけの行（"" だけ）も、
    -- cassava と同じく飛ばす
    rows line cs = case cs of
      []              -> []
      '\n' : rest     -> rows (line + 1) rest
      '\r' : '\n' : r -> rows (line + 1) r
      _               ->
        let (fields, line', rest) = fieldsFrom line cs
        in if fields == [(line, True)] then rows line' rest
           else map fst fields : rows line' rest
    -- The fields of one row: each starts on the line it is on now, and
    -- says if it is empty.
    -- 1行のフィールド。どれも、いまいる行から始まり、空かどうかを言う
    fieldsFrom line cs =
      let (line', empty, rest) = field line cs
          here = (line, empty)
      in case rest of
           ',' : more ->
             let (fs, l, r) = fieldsFrom line' more
             in (here : fs, l, r)
           '\n' : more        -> ([here], line' + 1, more)
           '\r' : '\n' : more -> ([here], line' + 1, more)
           _                  -> ([here], line', rest)
    -- One field: in quotes (line breaks inside are counted), or plain.
    -- フィールド1つ：引用符の中（中の改行も数える）か、そのまま
    field line cs = case cs of
      '"' : '"' : rest | ends rest -> (line, True, rest)
      '"' : rest -> plain False (quoted line rest)
      _          -> plain True (line, cs)
    quoted line cs = case cs of
      []             -> (line, [])
      '"' : '"' : r  -> quoted line r
      '"' : r        -> (line, r)
      '\n' : r       -> quoted (line + 1) r
      _ : r          -> quoted line r
    -- After the quotes (or with none): up to a comma or the line's end.
    -- 引用符のあと（か、引用符なし）：カンマか、行の終わりまで
    plain empty (line, cs)
      | ends cs   = (line, empty, cs)
      | otherwise = plain False (line, drop 1 cs)
    ends cs = case cs of
      []              -> True
      ',' : _         -> True
      '\n' : _        -> True
      '\r' : '\n' : _ -> True
      _               -> False
-- ANCHOR_END: csvLines

-- ANCHOR: tomlPlace
-- 📍 Toml.place: the path of a problem (.source[0].dir), as a line of
--    the file (line 5, source[0].dir). The path stays as it is when no
--    line has that key.
-- Toml.place：問題の道筋（.source[0].dir）を、ファイルの行で（line 5,
-- source[0].dir）。そのキーの行がなければ、道筋はそのまま
tomlPlaceBuiltin :: Builtin
tomlPlaceBuiltin =
  prim2 "Toml.place" (tString ~> tString ~> tString) $ \v p -> do
    text <- string v
    path <- string p
    Right $ VString $ case tomlLine text path of
      Just n  -> "line " <> T.pack (show n) <> ", " <> T.dropWhile (== '.') path
      Nothing -> path
-- ANCHOR_END: tomlPlace

-- ANCHOR: tomlLine
-- 🔎 The line (from 1) where a path is written: a key = value line
--    under its [table] or [[array of tables]] (counted from 0), or the
--    header of a table. A path into a value (.tags[1]) takes the line of
--    its key. Only the common shapes: inline tables and keys in quotes
--    with dots are not looked into.
-- 道筋が書いてある行（1 から）：[表] や [[表の配列]]（0 から数える）の下の
-- キー = 値 の行か、表の見出し。値の中への道筋（.tags[1]）は、そのキーの
-- 行。よくある形だけ：インラインの表や、点のある引用符のキーは見ない
tomlLine :: Text -> Text -> Maybe Int
tomlLine text path = best (steps path)
  where
    -- The line of the path, or else of the longest part of it.
    -- 道筋の行。なければ、いちばん長い前の部分の行
    best target
      | null target = Nothing
      | Just n <- lookup target written = Just n
      | otherwise = best (init target)
    written = go [] Map.empty (zip [1 ..] (T.lines text))
    go _ _ [] = []
    go header counts ((n, raw) : rest)
      | "[[" `T.isPrefixOf` line =
          let keys = dotted (T.dropEnd 2 (T.drop 2 line))
              i = Map.findWithDefault 0 keys counts
              here = map Named keys <> [At i]
          in (here, n) : go here (Map.insert keys (i + 1) counts) rest
      | "[" `T.isPrefixOf` line =
          let here = map Named (dotted (T.dropEnd 1 (T.drop 1 line)))
          in (here, n) : go here counts rest
      | (key, rest') <- T.breakOn "=" line, not (T.null rest')
      , not (T.null (T.strip key)) =
          (header <> map Named (dotted key), n) : go header counts rest
      | otherwise = go header counts rest
      where
        line = T.strip raw
    dotted = map (T.dropAround (`elem` ("\"'" :: String)) . T.strip)
           . T.splitOn "."

-- 🧭 A path as jq writes it, in steps:  .source[0].dir  is
--    source, 0, dir.
-- jq の書き方の道筋を、1歩ずつに
data Step = Named Text | At Int
  deriving (Eq, Show)

steps :: Text -> [Step]
steps = go . T.unpack
  where
    go cs = case cs of
      [] -> []
      '.' : rest -> go rest
      '[' : rest ->
        let (digits, more) = span isDigit rest
        in At (read digits) : go (drop 1 more)
      _ ->
        let (key, more) = break (`elem` (".[" :: String)) cs
        in Named (T.pack key) : go more
-- ANCHOR_END: tomlLine
-- ANCHOR_END: all
