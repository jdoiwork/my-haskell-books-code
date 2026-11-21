-- ANCHOR: header
{-# LANGUAGE TemplateHaskell #-}
-- ANCHOR: all
-- | The tables of Unicode 17.0, read from the files in ucd/ while the
--   program is built: the yes-or-no properties (\p{Emoji},
--   \p{Dash} ...), the bidi classes (\p{bc:AL}), and what \X needs
--   (the grapheme cluster break, and the Indic conjunct break).
-- Unicode 17.0 の表。ビルドするときに ucd/ のファイルから読む：
-- はい・いいえの性質（\p{Emoji}・\p{Dash} など）、双方向のクラス
-- （\p{bc:AL}）、\X に要るもの（書記素クラスターの区切りと、インドの
-- 文字の結合の区切り）
module MiniRegex.Ucd
  ( Table
  , inTable
  , binaryNamed
  , binaryNames
  , bidiClassNamed
  , bidiClassNames
  , GraphemeBreak (..)
  , graphemeBreak
  , Conjunct (..)
  , conjunct
  , isExtendedPictographic
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
-- The tables, as arrays of range ends
-- 表は、範囲の端を並べた配列
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import Data.Char (isAscii, ord, toLower)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.List (sort)
import Data.Maybe (fromMaybe)

-- Reading the files while building
-- ビルドするときにファイルを読む
import MiniRegex.UcdFile (shortBidi, ucdFile, ucdFileWith)
-- ANCHOR_END: imports

-- ANCHOR: table
-- 📏 A table: where the ranges start and end, in order
--    (lo1, hi1, lo2, hi2 ...).
-- 表：範囲の始まりと終わりを、順に並べたもの（lo1, hi1, lo2, hi2 ...）
type Table = UArray Int Int

-- 🧱 Make a table from the String the splice wrote: "AZaz" is A-Z
--    and a-z.
-- スプライスが書いた String から表を作る（"AZaz" は A-Z と a-z）
tableOf :: String -> Table
tableOf ends = listArray (0, length ends - 1) (map ord ends)

-- 🔍 Is the character in the table? A binary search: the first range
--    end that is not below the character must be the end of a range
--    that the character is in.
-- この文字は表に入っているか。二分探索：文字より小さくない最初の端が、
-- 文字の入っている範囲の終わりなら、入っている
inTable :: Table -> Char -> Bool
inTable t c = go 0 (n `div` 2)
  where
    (_, top) = bounds t
    n = top + 1
    x = ord c
    -- look among the ranges i .. j-1
    -- 範囲 i から j-1 までの中を探す
    go i j
      | i >= j    = False
      | otherwise =
          let k = (i + j) `div` 2
          in if x < t ! (2 * k) then go i k
             else if x > t ! (2 * k + 1) then go (k + 1) j
             else True
-- ANCHOR_END: table

-- ANCHOR: files
-- 📚 The files. Each splice reads one file while building.
-- ファイル。スプライスが、ビルドするときに1つずつ読む
emojiData, propList, coreProperties, mirroring, bidiClasses,
  graphemeBreaks :: [(String, String)]
emojiData        = $(ucdFile "ucd/emoji-data.txt")
propList         = $(ucdFile "ucd/PropList.txt")
coreProperties   = $(ucdFile "ucd/DerivedCoreProperties.txt")
mirroring        = $(ucdFile "ucd/BidiMirroring.txt")
bidiClasses      = $(ucdFileWith shortBidi "ucd/DerivedBidiClass.txt")
graphemeBreaks   = $(ucdFile "ucd/GraphemeBreakProperty.txt")
-- ANCHOR_END: files

-- ANCHOR: binary
-- 🏷️ The yes-or-no properties that PCRE2 10.49 knows, by their loose
--    names (no spaces, - or _, in lower case), long and short.
-- PCRE2 10.49 の知っている、はい・いいえの性質。ゆるい名前（空白・-・_
-- を抜いて小文字に）で、長い名前と短い名前の両方
binaryTables :: Map String Table
binaryTables = Map.fromList
  [ (loose name, tableOf ranges)
  | (name, ranges) <- emojiData ++ propList ++ coreProperties ]
  <> Map.fromList
  [ ("ascii", tableOf "\0\DEL")
  , ("incb", tableOf (incbAll coreProperties))
    -- ANCHOR: mirrored
    -- PCRE2's \p{Bidi_Mirrored} is the 428 characters that
    -- BidiMirroring.txt gives a mirror image to: not Unicode's 554
    -- (∑ and √ are Bidi_Mirrored, but have no mirror image)
    -- PCRE2 の \p{Bidi_Mirrored} は、BidiMirroring.txt が鏡に映した文字を
    -- 持たせている 428 文字。Unicode の 554 文字ではない（∑ や √ は
    -- Bidi_Mirrored だが、鏡に映した文字がない）
  , ("bidimirrored", tableOf (union (map snd mirroring))) ]
    -- ANCHOR_END: mirrored
  where
    -- \p{InCB}: Consonant, Extend or Linker (not None)
    -- \p{InCB}：Consonant・Extend・Linker のどれか（None ではない）
    incbAll ps = union [ r | (n, r) <- ps, take 6 n == "InCB; " ]

-- 🤝 The ranges of several Strings, in order, as one String. Ranges
--    next to each other become one.
-- いくつかの String の範囲を、順に並べて1つの String に。隣り合う範囲は1つに
union :: [String] -> String
union = concatMap (\(a, b) -> [a, b]) . joined . sort . concatMap pairs
  where
    pairs (a : b : more) = (a, b) : pairs more
    pairs _              = []
    joined ((a, b) : (c, d) : more)
      | succ b >= c = joined ((a, max b d) : more)
    joined (x : more) = x : joined more
    joined [] = []

-- 🔎 The table of \p{name}, if PCRE2 knows the name.
-- \p{name} の表（PCRE2 がその名前を知っていれば）
binaryNamed :: String -> Maybe (String, Table)
binaryNamed written = do
  long <- Map.lookup (loose written) aliases
  table <- Map.lookup long binaryTables
  pure (long, table)

-- 📇 Every loose name, short or long, and the long name it stands for.
-- ゆるい名前（短くても長くても）と、それが表す長い名前
aliases :: Map String String
aliases = Map.fromList
  ([ (n, n) | n <- binaryNames ]
     ++ [ (loose short, loose long) | (short, long) <- shortNames ])

-- 📋 The long names of all the yes-or-no properties (pcre2test -LP).
-- はい・いいえの性質の長い名前の全部（pcre2test -LP）
binaryNames :: [String]
binaryNames = map loose
  [ "ASCII", "ASCII_Hex_Digit", "Alphabetic", "Bidi_Control"
  , "Bidi_Mirrored", "Cased", "Case_Ignorable", "Changes_When_Casefolded"
  , "Changes_When_Casemapped", "Changes_When_Lowercased"
  , "Changes_When_Titlecased", "Changes_When_Uppercased", "Dash"
  , "Default_Ignorable_Code_Point", "Deprecated", "Diacritic", "Emoji"
  , "Emoji_Component", "Emoji_Modifier", "Emoji_Modifier_Base"
  , "Emoji_Presentation", "Extended_Pictographic", "Extender"
  , "Grapheme_Base", "Grapheme_Extend", "Grapheme_Link", "Hex_Digit"
  , "ID_Compat_Math_Continue", "ID_Compat_Math_Start", "ID_Continue"
  , "ID_Start", "IDS_Binary_Operator", "IDS_Trinary_Operator"
  , "IDS_Unary_Operator", "Ideographic", "InCB", "Join_Control"
  , "Logical_Order_Exception", "Lowercase", "Math"
  , "Modifier_Combining_Mark", "Noncharacter_Code_Point"
  , "Pattern_Syntax", "Pattern_White_Space"
  , "Prepended_Concatenation_Mark", "Quotation_Mark", "Radical"
  , "Regional_Indicator", "Sentence_Terminal", "Soft_Dotted"
  , "Terminal_Punctuation", "Unified_Ideograph", "Uppercase"
  , "Variation_Selector", "White_Space", "XID_Continue", "XID_Start" ]

-- 🩳 The short names (pcre2test -LP shows them in parentheses).
-- 短い名前（pcre2test -LP が、かっこの中に見せる）
shortNames :: [(String, String)]
shortNames =
  [ ("AHex", "ASCII_Hex_Digit"), ("Alpha", "Alphabetic")
  , ("Bidi_C", "Bidi_Control"), ("Bidi_M", "Bidi_Mirrored")
  , ("CI", "Case_Ignorable"), ("CWCF", "Changes_When_Casefolded")
  , ("CWCM", "Changes_When_Casemapped"), ("CWL", "Changes_When_Lowercased")
  , ("CWT", "Changes_When_Titlecased"), ("CWU", "Changes_When_Uppercased")
  , ("DI", "Default_Ignorable_Code_Point"), ("Dep", "Deprecated")
  , ("Dia", "Diacritic"), ("EBase", "Emoji_Modifier_Base")
  , ("EComp", "Emoji_Component"), ("EMod", "Emoji_Modifier")
  , ("EPres", "Emoji_Presentation"), ("Ext", "Extender")
  , ("ExtPict", "Extended_Pictographic"), ("Gr_Base", "Grapheme_Base")
  , ("Gr_Ext", "Grapheme_Extend"), ("Gr_Link", "Grapheme_Link")
  , ("Hex", "Hex_Digit"), ("IDC", "ID_Continue"), ("Ideo", "Ideographic")
  , ("IDS", "ID_Start"), ("IDSB", "IDS_Binary_Operator")
  , ("IDST", "IDS_Trinary_Operator"), ("IDSU", "IDS_Unary_Operator")
  , ("Join_C", "Join_Control"), ("LOE", "Logical_Order_Exception")
  , ("Lower", "Lowercase"), ("MCM", "Modifier_Combining_Mark")
  , ("NChar", "Noncharacter_Code_Point"), ("Pat_Syn", "Pattern_Syntax")
  , ("Pat_WS", "Pattern_White_Space")
  , ("PCM", "Prepended_Concatenation_Mark"), ("QMark", "Quotation_Mark")
  , ("RI", "Regional_Indicator"), ("SD", "Soft_Dotted")
  , ("STerm", "Sentence_Terminal"), ("Term", "Terminal_Punctuation")
  , ("UIdeo", "Unified_Ideograph"), ("Upper", "Uppercase")
  , ("VS", "Variation_Selector"), ("Space", "White_Space")
  , ("WSpace", "White_Space"), ("XIDC", "XID_Continue")
  , ("XIDS", "XID_Start") ]

-- 🪶 PCRE2's loose way of reading a name: no spaces, - or _, and the
--    case does not count.
-- PCRE2 のゆるい名前の読み方：空白・-・_ を抜き、大文字と小文字も区別しない
loose :: String -> String
loose = map toLower . filter (`notElem` (" \t\n\v\f\r-_" :: String))
-- ANCHOR_END: binary

-- ANCHOR: bidi
-- ↔️ \p{bc:AL}: the bidi classes, by their short names only (as in
--    PCRE2: \p{bc:Arabic_Letter} is error 147).
-- \p{bc:AL}：双方向のクラスを、短い名前だけで（PCRE2 と同じく、
-- \p{bc:Arabic_Letter} はエラー 147）
bidiTables :: Map String Table
bidiTables = Map.fromList
  [ (map toLower name, tableOf ranges) | (name, ranges) <- bidiClasses ]

bidiClassNamed :: String -> Maybe (String, Table)
bidiClassNamed written = do
  let name = loose written
  table <- Map.lookup name bidiTables
  pure (name, table)

bidiClassNames :: [String]
bidiClassNames = Map.keys bidiTables
-- ANCHOR_END: bidi

-- ANCHOR: grapheme
-- 🧩 The values of Grapheme_Cluster_Break that \X looks at (UAX #29).
--    A character that the file does not list is Other.
-- \X が見る Grapheme_Cluster_Break の値（UAX #29）。ファイルにない
-- 文字は Other
data GraphemeBreak
  = CR | LF | Control | Extend | ZWJ | RegionalIndicator | Prepend
  | SpacingMark | L | V | T | LV | LVT | Other
  deriving (Show, Eq, Ord, Enum, Bounded)

-- 🧩 The value of a character. The ASCII ones are quick.
-- 文字の値。ASCII の文字は近道で
graphemeBreak :: Char -> GraphemeBreak
graphemeBreak c
  | c == '\r'         = CR
  | c == '\n'         = LF
  | isAscii c         = if c < ' ' || c == '\DEL' then Control else Other
  | otherwise = fromMaybe Other (lookup True
      [ (inTable t c, v) | (v, t) <- breakTables ])

breakTables :: [(GraphemeBreak, Table)]
breakTables =
  [ (v, tableOf ranges)
  | (name, ranges) <- graphemeBreaks
  , Just v <- [lookup name names] ]
  where
    names =
      [ ("CR", CR), ("LF", LF), ("Control", Control), ("Extend", Extend)
      , ("ZWJ", ZWJ), ("Regional_Indicator", RegionalIndicator)
      , ("Prepend", Prepend), ("SpacingMark", SpacingMark), ("L", L)
      , ("V", V), ("T", T), ("LV", LV), ("LVT", LVT) ]

-- 🪢 Indic_Conjunct_Break (for the rule GB9c): a consonant, a linker
--    (the virama that joins two consonants), an extend, or none.
-- Indic_Conjunct_Break（規則 GB9c のため）：子音、つなぐもの（2つの子音を
-- つなぐヴィラーマ）、Extend、どれでもない
data Conjunct = Consonant | Linker | ConjunctExtend | NoConjunct
  deriving (Show, Eq)

conjunct :: Char -> Conjunct
conjunct c
  | isAscii c = NoConjunct
  | inTable consonants c = Consonant
  | inTable linkers c = Linker
  | inTable extends c = ConjunctExtend
  | otherwise = NoConjunct

consonants, linkers, extends :: Table
consonants = incb "Consonant"
linkers    = incb "Linker"
extends    = incb "Extend"

incb :: String -> Table
incb v = tableOf (fromMaybe "" (lookup ("InCB; " ++ v) coreProperties))

-- 🖼️ Extended_Pictographic (for the rule GB11: emoji joined by ZWJ).
-- Extended_Pictographic（規則 GB11 のため：ZWJ でつないだ絵文字）
isExtendedPictographic :: Char -> Bool
isExtendedPictographic c = not (isAscii c) && inTable extPict c

extPict :: Table
extPict = tableOf (fromMaybe "" (lookup "Extended_Pictographic" emojiData))
-- ANCHOR_END: grapheme
-- ANCHOR_END: all
