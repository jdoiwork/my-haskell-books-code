-- ANCHOR: all
-- ANCHOR: head
-- | The answers to the training: the design chapter's, and then the
--   Unicode chapter's.
-- トレーニングの解答：設計の章のものと、Unicode の章のもの
module RegexAnswers
  ( colourTree
  , choiceTree
  , escape
  , asciiDigits
  , charOffset
  ) where

import Data.Char (GeneralCategory (..), chr, generalCategory, ord)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Foreign (lengthWord8)

import MiniRegex
import MiniRegex.Syntax
-- ANCHOR_END: head

-- The design chapter's training
-- 設計の章のトレーニング

-- 🌈 colou?r: c, o, l, o, then u zero or one time, then r.
-- colou?r：c・o・l・o、u を 0 回か 1 回、そして r
colourTree :: Pattern
colourTree = Pattern (Branch pieces :| [])
  where
    pieces =
      [ Quantified (Lit 'c') once
      , Quantified (Lit 'o') once
      , Quantified (Lit 'l') once
      , Quantified (Lit 'o') once
      , Quantified (Lit 'u') optional
      , Quantified (Lit 'r') once
      ]

-- 🍀 (?:ab)+|c: two branches. The left one is a group that does not
--    keep what it found, one time or more.
-- (?:ab)+|c：枝が2本。左は、見つけたものを覚えないグループを1回以上
choiceTree :: Pattern
choiceTree = Pattern (left :| [right])
  where
    left  = Branch [Quantified (Group NonCapturing ab) plus]
    ab    = Pattern (Branch [ Quantified (Lit 'a') once
                            , Quantified (Lit 'b') once ] :| [])
    right = Branch [Quantified (Lit 'c') once]

-- 🛡️ Put \ before every character with a meaning in a pattern, so
--    that the pattern finds the text just as it is.
-- パターンの中で意味を持つ文字の前に \ を付けて、その文字列そのものを
-- 探すパターンにする
escape :: Text -> Text
escape = T.concatMap one
  where
    one c
      | c `elem` special = T.pack ['\\', c]
      | otherwise        = T.singleton c
    special = "\\^$.|?*+()[]{}" :: String

-- The Unicode chapter's training
-- Unicode の章のトレーニング

-- ANCHOR: digits
-- 🔢 Every decimal digit (２, ٣, 𝟘 ...) into 0 to 9. In Unicode, the
--    digits of each kind sit side by side, from 0 to 9; so the value
--    of a digit is how many digits come just before it, modulo 10.
-- どの10進の数字（２・٣・𝟘 など）も 0 から 9 にする。Unicode では、どの
-- 種類の数字も、0 から 9 の順に並んでいる。だから、数字の値は、すぐ前に
-- 続いている数字の数を 10 で割った余り
asciiDigits :: Text -> Text
asciiDigits = replaceWith digit (T.singleton . toAscii . T.head . matchText)
  where
    digit = either (error . show) id (compile "\\d")
    toAscii c = chr (ord '0' + value c)
    value c = length (takeWhile isDigitChar (drop 1 [c, pred c ..])) `mod` 10
    isDigitChar x = generalCategory x == DecimalNumber
-- ANCHOR_END: digits

-- ANCHOR: offset
-- 📍 The place in characters of a place in bytes (from PCRE2, Rust or
--    Go). Nothing in the middle of a character, or past the end.
-- バイトで数えた場所（PCRE2・Rust・Go から来たもの）を、文字で数えた
-- 場所にする。文字の途中や、終わりより先なら Nothing
charOffset :: Text -> Int -> Maybe Int
charOffset t b = lookup b (zip starts [0 ..])
  where
    -- the byte where each character starts, and the end
    -- それぞれの文字が始まるバイトと、終わり
    starts = scanl (+) 0 (map (lengthWord8 . T.singleton) (T.unpack t))
-- ANCHOR_END: offset
-- ANCHOR_END: all
