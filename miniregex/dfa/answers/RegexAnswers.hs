-- ANCHOR: all
-- ANCHOR: head
-- | The answers to the training: the design chapter's, and then the
--   chapter on the lazy DFA.
-- トレーニングの解答：設計の章のものと、lazy DFA の章のもの
module RegexAnswers
  ( colourTree
  , choiceTree
  , escape
  , minLength
  , findAllLong
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex
import MiniRegex.Syntax
import MiniRegex.Types (Regex (..))
import MiniRegex.Width (Width (..), patternWidth)
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

-- The chapter on the lazy DFA
-- lazy DFA の章

-- ANCHOR: minlength
-- 📏 PCRE2's MINLENGTH: every match is at least this many characters
--    long (the short end of the pattern's width).
-- PCRE2 の MINLENGTH：どの一致も、少なくともこれだけの文字の長さ
-- （パターンの幅の、短いほうの端）
minLength :: Regex -> Int
minLength re = fromIntegral (shortest (patternWidth (tree re)))

-- 📏 findAll, but a text shorter than that is not searched at all.
-- findAll。ただし、それより短い文字列は、探しもしない
findAllLong :: Regex -> Text -> [Match]
findAllLong re t
  | T.compareLength t (minLength re) == LT = []
  | otherwise                              = findAll re t
-- ANCHOR_END: minlength
-- ANCHOR_END: all
