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
  ) where

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
toolboxBuiltins = stringNames <> [numberedRowsBuiltin]
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
-- ANCHOR_END: all
