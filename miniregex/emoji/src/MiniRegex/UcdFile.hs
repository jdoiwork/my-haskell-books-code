-- ANCHOR: all
-- ANCHOR: header
-- | Reading Unicode's data files (the UCD) while the program is built,
--   with Template Haskell. A file like emoji-data.txt says, line by
--   line, which characters have which value:
--
--   > 1F600..1F64F  ; Emoji_Presentation  # 😀..🙏
--
--   'ucdFile' reads such a file, and writes, for each value, the
--   ranges of the characters that have it.
-- Unicode のデータのファイル（UCD）を、プログラムをビルドするときに
-- Template Haskell で読む。emoji-data.txt のようなファイルには、どの文字が
-- どの値を持つかが、1行ずつ書いてある。ucdFile はそういうファイルを読んで、
-- 値ごとに、その値を持つ文字の範囲を書き出す
module MiniRegex.UcdFile
  ( Entry (..)
  , readEntries
  , paint
  , ucdFile
  , ucdFileWith
  , shortBidi
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
-- Template Haskell: running code while the program is built
-- Template Haskell：プログラムをビルドするときにコードを動かす
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, lift)

-- Reading the lines
-- 行を読む
import Data.Char (chr, isSpace)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Numeric (readHex)
-- ANCHOR_END: imports

-- ANCHOR: entry
-- 📄 One line of a file: the characters from lo to hi have the value
--    (what is after the first ;, with ; between the fields:
--    "InCB; Linker"). A line "# @missing: ..." gives the value of the
--    characters that no other line lists.
-- ファイルの1行：lo から hi までの文字が、その値を持つ（最初の ; のあとを、
-- 欄のあいだに ; を入れて："InCB; Linker"）。"# @missing: ..." の行は、
-- ほかのどの行にもない文字の値を言う
data Entry = Entry
  { lo      :: Int
  , hi      :: Int
  , value   :: String
  , missing :: Bool
  }
  deriving Show

-- 🔎 Every line of a file that says something: not the comments, not
--    the empty lines (but the @missing ones).
-- ファイルの、何かを言っている行の全部。コメントと空の行は飛ばす
-- （@missing の行は残す）
readEntries :: String -> [Entry]
readEntries = concatMap entry . lines
  where
    entry line = case line of
      '#' : ' ' : '@' : 'm' : 'i' : 's' : 's' : 'i' : 'n' : 'g' : ':' : r
        -> [ (fields r) { missing = True } ]
      '#' : _ -> []
      _ | all isSpace line -> []
        | otherwise -> [fields (takeWhile (/= '#') line)]
    fields text =
      let (range, after) = break (== ';') text
          (a, b) = case break (== '.') (trim range) of
            (x, '.' : '.' : y) -> (x, y)
            (x, _)             -> (x, x)
          values = splitOn ';' (drop 1 after)
      in Entry (hex a) (hex b) (joinWith "; " (map trim values)) False
    hex s = case readHex s of
      [(n, "")] -> n
      _         -> error ("not a code point: " ++ s)
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    splitOn c s = case break (== c) s of
      (x, [])    -> [x]
      (x, _ : y) -> x : splitOn c y
    joinWith sep = foldr1 (\x y -> x ++ sep ++ y)
-- ANCHOR_END: entry

-- ANCHOR: paint
-- 🖌️ Each character's value, as ranges: start with no value, and
--    paint the lines over it in order, the @missing ones first. A later
--    line paints over an earlier one (DerivedBidiClass.txt says the
--    Hebrew block is R by default, over "everything is L").
-- 文字ごとの値を、範囲で：値のない所から始めて、行を順に塗り重ねる
-- （@missing の行が先）。あとの行が、前の行の上に塗る
-- （DerivedBidiClass.txt は「全部 L」の上に「ヘブライ文字の区画は R」と言う）
paint :: [Entry] -> [(Int, Int, String)]
paint entries = Map.foldrWithKey spans [] painted
  where
    ordered = filter missing entries ++ filter (not . missing) entries
    painted = foldl over Map.empty ordered
    -- the map: where a run starts -> (where it ends, its value).
    -- The runs never overlap.
    -- 地図：ひと続きの始まり -> （終わり、値）。ひと続きは重ならない
    over m (Entry a b v _) =
      let (below, atA, aboveA) = Map.splitLookup a m
          (inside, above) = Map.spanAntitone (<= b) aboveA
          -- a run that starts before a keeps its part before a, and
          -- its part after b
          -- a より前で始まるひと続きは、a の前と b のあとが残る
          belowCut = case Map.lookupMax below of
            Just (s, (e, w)) | e >= a ->
              Map.insert s (a - 1, w) below <> after e w
            _ -> below
          -- the last run that starts inside keeps its part after b
          -- 中で始まる最後のひと続きは、b のあとが残る
          starts = maybe [] (\r -> [(a, r)]) atA ++ Map.toList inside
          insideCut = case reverse starts of
            (_, (e, w)) : _ -> after e w
            []              -> mempty
          after e w
            | e > b     = Map.singleton (b + 1) (e, w)
            | otherwise = mempty
      in Map.insert a (b, v) (belowCut <> insideCut <> above)
    spans s (e, v) acc = (s, e, v) : acc
-- ANCHOR_END: paint

-- ANCHOR: splice
-- 📦 Read the file while building, and write an expression of type
--    [(String, String)]: each value, with its ranges. The ranges are a
--    String of characters two by two: "AZaz" is A-Z and a-z (a code
--    point fits in a Char, and the whole table is one literal).
--    In emoji-data.txt and PropList.txt, a character can have many
--    values (U+200E is Bidi_Control and Pattern_White_Space too), so
--    the lines of each value are just put together.
-- ビルドするときにファイルを読んで、[(String, String)] の式を書く：値ごとに、
-- その範囲。範囲は、文字を2つずつ並べた String（"AZaz" は A-Z と a-z。
-- コードポイントは Char に収まり、表の全体が1つのリテラルになる）
-- emoji-data.txt や PropList.txt では、1つの文字がいくつもの値を持てる
-- （U+200E は Bidi_Control でもあり Pattern_White_Space でもある）ので、
-- 値ごとに行を集めるだけ
ucdFile :: FilePath -> Q Exp
ucdFile path = do
  entries <- entriesOf path
  lift (grouped [ (lo e, hi e, value e) | e <- entries, not (missing e) ])

-- 🖌️ For a file where each character has just one value
--    (DerivedBidiClass.txt): paint, with each value renamed first
--    (Left_To_Right -> L).
-- 文字ごとに値が1つだけのファイル（DerivedBidiClass.txt）のため：
-- 塗り重ねる。先に値の名前を付け替える（Left_To_Right -> L）
ucdFileWith :: (String -> String) -> FilePath -> Q Exp
ucdFileWith rename path = do
  entries <- entriesOf path
  lift (grouped (paint [ e { value = rename (value e) } | e <- entries ]))

-- 📄 The lines of the file. Build again when the file changes.
-- ファイルの行。ファイルが変わったら、ビルドし直す
entriesOf :: FilePath -> Q [Entry]
entriesOf path = do
  addDependentFile path
  readEntries <$> runIO (readFile path)

-- 🧺 Each value, with its ranges in order (as a String, two by two).
--    Ranges next to each other become one.
-- 値ごとに、範囲を順に（2文字ずつの String で）。隣り合う範囲は1つに
grouped :: [(Int, Int, String)] -> [(String, String)]
grouped spans =
  [ (v, concat [ [chr a, chr b] | (a, b) <- joined (sortOn fst rs) ])
  | (v, rs) <- Map.toList byValue ]
  where
    byValue = Map.fromListWith (++) [ (v, [(a, b)]) | (a, b, v) <- spans ]
    joined ((a, b) : (c, d) : more)
      | b + 1 >= c = joined ((a, max b d) : more)
    joined (x : more) = x : joined more
    joined [] = []
-- ANCHOR_END: splice

-- ANCHOR: short
-- 🏷️ The long names that only the @missing lines of
--    DerivedBidiClass.txt use. (It is here, not next to the splice:
--    a splice can only run functions from another module.)
-- DerivedBidiClass.txt の @missing の行だけが使う、長い名前。（スプライスの
-- 隣ではなく、ここに置く。スプライスが動かせるのは、ほかのモジュールの
-- 関数だけ）
shortBidi :: String -> String
shortBidi v = case v of
  "Left_To_Right"       -> "L"
  "Right_To_Left"       -> "R"
  "Arabic_Letter"       -> "AL"
  "European_Terminator" -> "ET"
  _                     -> v
-- ANCHOR_END: short

-- ANCHOR_END: all
