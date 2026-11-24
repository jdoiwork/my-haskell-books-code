-- ANCHOR: all
-- | What MiniRegex knows about Unicode: which characters are the same
--   but for case, which ones are in \d, \w, [:alpha:] and \p{...}, and
--   which ones the flag x skips.
-- MiniRegex が Unicode について知っていること：大文字と小文字のちがい
-- だけの文字はどれか、\d・\w・[:alpha:]・\p{...} に入る文字はどれか、
-- フラグ x が読み飛ばす文字はどれか
module MiniRegex.Unicode
  ( fold
  , sameCase
  , caseVariants
  , hasType
  , propertyNamed
  , posixNames
  , isPatternSpace
  ) where

-- ANCHOR: imports
-- Characters and their categories, and the scripts
-- 文字とその一般カテゴリー、文字体系
import Data.Char
  ( GeneralCategory (..), generalCategory, isAsciiLower, isAsciiUpper
  , chr, isDigit, isHexDigit, ord, toLower, toUpper )
import Unicode.Char.General.Scripts (Script (Coptic, Inherited), script,
  scriptExtensions, scriptShortName)

-- Tables
-- 表
import Data.List (sort)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex.Syntax (CharType (..), Property (..), Reach (..))
-- ANCHOR_END: imports

-- ANCHOR: fold
-- 🪗 The one character that stands for all the characters that are
--    the same but for case: a for a and A, k for k, K and K (the
--    Kelvin sign), s for s, S and ſ. Mostly the lower case of the
--    upper case; a few characters are the other way, as PCRE2 has them
--    (from Unicode's CaseFolding.txt).
-- 大文字と小文字のちがいだけの文字の、代表の1文字：a と A なら a、
-- k・K・K（ケルビン記号）なら k、s・S・ſ なら s。たいていは、大文字に
-- してから小文字にしたもの。いくつかの文字だけ、PCRE2 と同じく例外
-- （Unicode の CaseFolding.txt から）
fold :: Char -> Char
fold c = case c of
  '\x130'  -> c         -- İ: its lower case is i, but not the other way
  '\x131'  -> c         -- ı: its upper case is I, but not the other way
  '\x1FD3' -> '\x390'   -- ΐ: the same letter as U+0390
  '\x1FE3' -> '\x3B0'   -- ΰ: the same letter as U+03B0
  '\xFB06' -> '\xFB05'  -- ﬆ: the same ligature as ﬅ
  _        -> toLower (toUpper c)

-- 🤝 Are the two characters the same, but for case?
-- 2つの文字は、大文字と小文字のちがいだけか
sameCase :: Char -> Char -> Bool
sameCase a b = a == b || fold a == fold b

-- 👪 All the characters that are the same as this one, but for case
--    (itself too).
-- この文字と、大文字と小文字のちがいだけの文字の全部（自分も入る）
caseVariants :: Char -> [Char]
caseVariants c = IntMap.findWithDefault [c] (ord (fold c)) caseSets

-- 📚 The sets of more than one character, by their fold. Made once,
--    the first time it is needed, by looking at every character.
-- 2文字以上の組を、代表の文字ごとに。いちばん最初に要ったときに1回だけ、
-- 全部の文字を見て作る
caseSets :: IntMap [Char]
caseSets = IntMap.mapWithKey (\k cs -> sort (chr k : cs)) others
  where
    -- every character whose fold is another one, under that one
    -- 代表がほかの文字である文字を、その代表の下に
    others = IntMap.fromListWith (++)
      [ (ord (fold c), [c])
      | c <- [minBound .. maxBound]
      , not (c >= '\xD800' && c <= '\xDFFF')
      , fold c /= c ]
-- ANCHOR_END: fold

-- ANCHOR: types
-- 🧺 Is the character in the set with this name?
-- この文字は、この名前の集まりに入っているか
hasType :: CharType -> Char -> Bool
hasType t c = case t of
  Digit AsciiOnly  -> isDigit c
  Digit AllUnicode -> category == DecimalNumber
  WhiteSpace AsciiOnly  -> c `elem` asciiSpaces
  WhiteSpace AllUnicode -> isSeparator || hasType HSpace c || hasType VSpace c
  Word AsciiOnly   -> isAsciiWord c
  Word AllUnicode  -> isLetter || isNumber || category `elem`
                        [NonSpacingMark, ConnectorPunctuation]
  HSpace           -> c `elem` horizontalSpaces
  VSpace           -> c `elem` verticalSpaces
  Posix reach name -> posix reach name c
  Property p       -> hasProperty p c
  where
    category    = generalCategory c
    isLetter    = category <= OtherLetter
    isNumber    = category >= DecimalNumber && category <= OtherNumber
    isSeparator = category >= Space && category <= ParagraphSeparator

-- 🔤 The ASCII ones.
-- ASCII のもの
asciiSpaces :: [Char]
asciiSpaces = "\t\n\v\f\r "

isAsciiWord :: Char -> Bool
isAsciiWord c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'

-- 📏 \h and \v: the same lists in PCRE2, with or without Unicode.
-- \h と \v：PCRE2 では、Unicode があってもなくても同じ表
horizontalSpaces, verticalSpaces :: [Char]
horizontalSpaces =
  "\t \xA0\x1680\x180E" ++ ['\x2000' .. '\x200A'] ++ "\x202F\x205F\x3000"
verticalSpaces = "\n\v\f\r\x85\x2028\x2029"
-- ANCHOR_END: types

-- ANCHOR: posix
-- 🏛️ The POSIX names in [[:alpha:]], and what they mean. With ASCII
--    only, no character from 128 on is in any of them. With Unicode,
--    PCRE2 reads most of them as \p{...}; ascii, blank... stay the same.
-- [[:alpha:]] の中の POSIX の名前と、その意味。ASCII だけなら、128 から
-- 先の文字は、どれにも入らない。Unicode なら、PCRE2 はたいていを
-- \p{...} と読み替える。ascii などは、そのまま
posixNames :: [Text]
posixNames =
  [ "alnum", "alpha", "ascii", "blank", "cntrl", "digit", "graph"
  , "lower", "print", "punct", "space", "upper", "word", "xdigit" ]

posix :: Reach -> Text -> Char -> Bool
posix AsciiOnly name c = ord c < 128 && case name of
  "alnum"  -> isAlnum
  "alpha"  -> isAlpha
  "ascii"  -> True
  "blank"  -> c == ' ' || c == '\t'
  "cntrl"  -> ord c < 32 || ord c == 127
  "digit"  -> isDigit c
  "graph"  -> c > ' ' && c < '\DEL'
  "lower"  -> isAsciiLower c
  "print"  -> c >= ' ' && c < '\DEL'
  "punct"  -> c > ' ' && c < '\DEL' && not isAlnum
  "space"  -> c `elem` asciiSpaces
  "upper"  -> isAsciiUpper c
  "word"   -> isAsciiWord c
  "xdigit" -> isHexDigit c
  _        -> False
  where
    isAlpha = isAsciiLower c || isAsciiUpper c
    isAlnum = isAlpha || isDigit c
posix AllUnicode name c = case name of
  "alnum"  -> property "Xan"
  "alpha"  -> category "L"
  "blank"  -> hasType HSpace c
  "cntrl"  -> category "Cc"
  "digit"  -> category "Nd"
  "lower"  -> category "Ll"
  "space"  -> property "Xps"
  "upper"  -> category "Lu"
  "word"   -> property "Xwd"
  -- letters, marks, numbers, punctuation, symbols, and the format
  -- characters, but for some invisible marks. (The manual says that
  -- print is graph and Zs, but PCRE2 10.49 leaves U+180E out of graph
  -- only.)
  -- 文字・結合文字・数字・句読点・記号・書式の文字。ただし、目に見えない
  -- 印のいくつかは外す（説明書は「print は graph と Zs」と言うが、
  -- PCRE2 10.49 が U+180E を外すのは graph だけ）
  "graph"  -> graphic && c /= '\x180E'
  "print"  -> graphic || generalCategory c == Space
  -- (the manual says "S below 256", but PCRE2 10.49 takes S below 128)
  -- （説明書は「256 より下の S」と言うが、PCRE2 10.49 は 128 より下）
  "punct"  -> category "P" || (category "S" && ord c < 128)
  "xdigit" -> isHexDigit c || fullWidthHex
  _        -> posix AsciiOnly name c
  where
    category n = hasProperty (Category n) c
    property n = hasProperty (Special n) c
    graphic = any category ["L", "M", "N", "P", "S", "Cf"]
                && c `notElem` ('\x61C' : ['\x2066' .. '\x2069'])
    fullWidthHex = c `elem` (['\xFF10' .. '\xFF19'] ++ ['\xFF21' .. '\xFF26']
                               ++ ['\xFF41' .. '\xFF46'])
-- ANCHOR_END: posix

-- ANCHOR: property
-- 🏷️ Is the character in \p{...}?
-- この文字は \p{...} に入っているか
hasProperty :: Property -> Char -> Bool
hasProperty p c = case p of
  Category name   -> generalCategory c `elem` categoriesOf name
  Script s        -> script c == s
  ScriptExt s     -> script c == s || s `elem` scriptExtensions c
  AnyCode         -> True
  Special "Xan"   -> letter || number
  -- (the manual says "tab ... CR and Z", but PCRE2 10.49 takes \x85
  -- and U+180E too: the same as \s with Unicode)
  -- （説明書は「タブから CR までと Z」と言うが、PCRE2 10.49 は \x85 と
  -- U+180E も取る。Unicode の \s と同じ）
  Special "Xps"   -> hasType (WhiteSpace AllUnicode) c
  Special "Xsp"   -> hasType (WhiteSpace AllUnicode) c
  Special "Xwd"   -> letter || number
                       || hasProperty (Category "Mn") c
                       || hasProperty (Category "Pc") c
  -- the characters a universal character name (\u00E9 in C) can
  -- stand for: $ @ ` and from U+00A0 on (Char has no surrogates)
  -- 「ユニバーサル文字名」（C の \u00E9）で書ける文字：$ @ ` と、U+00A0
  -- から先（Char にサロゲートはない）
  Special "Xuc"   -> c `elem` ("$@`" :: String) || c >= '\xA0'
  Special _       -> False
  where
    letter    = hasProperty (Category "L") c
    number    = hasProperty (Category "N") c

-- 📇 The general categories, by their two-letter names. A name of one
--    letter is all the categories that start with it; Lc (or L&) is
--    the letters that have case.
-- 一般カテゴリーを、2文字の名前で。1文字の名前は、その文字で始まる
-- カテゴリーの全部。Lc（L& とも書く）は、大文字と小文字のある文字
categoriesOf :: Text -> [GeneralCategory]
categoriesOf name = case T.unpack name of
  [_]  -> [g | (n, g) <- categoryNames, T.take 1 n == name]
  "Lc" -> [UppercaseLetter, LowercaseLetter, TitlecaseLetter]
  _    -> [g | (n, g) <- categoryNames, n == name]

categoryNames :: [(Text, GeneralCategory)]
categoryNames = zip
  [ "Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Mc", "Me", "Nd", "Nl", "No"
  , "Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po", "Sm", "Sc", "Sk", "So"
  , "Zs", "Zl", "Zp", "Cc", "Cf", "Cs", "Co", "Cn" ]
  [minBound .. maxBound]

-- 🔎 What is written in \p{...}, as a Property. As PCRE2 does, spaces,
--    - and _ are left out, and the case does not count: \p{ sc : Han }
--    is \p{sc:han}. A script on its own (\p{Han}) is the script or one
--    it is used with; sc: is the script only. Nothing for a name that
--    MiniRegex does not know.
-- \p{...} に書いてあるものを、Property にする。PCRE2 と同じく、空白と
-- - と _ は抜いて、大文字と小文字も区別しない：\p{ sc : Han } は
-- \p{sc:han}。文字体系の名前だけ（\p{Han}）なら、その文字体系か、一緒に
-- 使われる文字体系。sc: なら、その文字体系だけ。MiniRegex の知らない
-- 名前なら Nothing
propertyNamed :: Text -> Maybe Property
propertyNamed written = case T.breakOn ":" loose of
  (kind, value)
    | Just v <- T.stripPrefix ":" value -> case kind of
        _ | kind `elem` ["sc", "script"] -> Script <$> scriptNamed v
          | kind `elem` ["scx", "scriptextensions"] ->
              ScriptExt <$> scriptNamed v
          | otherwise -> Nothing
  _ | loose `elem` ["any"] -> Just AnyCode
    | loose `elem` ["l&", "lc"] -> Just (Category "Lc")
    | Just n <- lookup loose categories -> Just (Category n)
    | Just n <- lookup loose specials -> Just (Special n)
    | otherwise -> ScriptExt <$> scriptNamed loose
  where
    loose = T.toLower (T.filter (`notElem` (" \t\n\v\f\r-_" :: String))
                                (T.replace "=" ":" written))
    categories = [ (T.toLower n, n)
                 | n <- map fst categoryNames
                          ++ ["C", "L", "M", "N", "P", "S", "Z"] ]
    specials = [ (T.toLower n, n)
               | n <- ["Xan", "Xps", "Xsp", "Xuc", "Xwd"] ]

-- 📜 A script, by its long name (Hiragana) or its four letters (Hira).
-- 文字体系を、長い名前（Hiragana）か4文字の名前（Hira）で
scriptNamed :: Text -> Maybe Script
scriptNamed name = Map.lookup name scriptsByName

scriptsByName :: Map Text Script
scriptsByName = Map.fromList
  (concat [ [(T.toLower (T.pack (show s)), s)
            , (T.toLower (T.pack (scriptShortName s)), s)]
          | s <- [minBound .. maxBound] ]
     -- two more short names that Unicode keeps for old times
     -- Unicode が昔のために残している、もう2つの短い名前
     ++ [("qaai", Inherited), ("qaac", Coptic)])
-- ANCHOR_END: property

-- ANCHOR: space
-- 💤 The characters that the flag x skips in a pattern: Unicode's
--    Pattern_White_Space (PCRE2 with UTF). Not the no-break space.
-- フラグ x がパターンの中で読み飛ばす文字：Unicode の
-- Pattern_White_Space（UTF のある PCRE2 と同じ）。ノーブレークスペースは
-- 入らない
isPatternSpace :: Char -> Bool
isPatternSpace c =
  c `elem` ("\t\n\v\f\r \x85\x200E\x200F\x2028\x2029" :: String)
-- ANCHOR_END: space
-- ANCHOR_END: all
