-- ANCHOR: all
-- ANCHOR: head
-- | The answers to the training: the design chapter's, and then the
--   chapter on looking around's.
-- トレーニングの解答：設計の章のものと、まわりを見る章のもの
module RegexAnswers
  ( colourTree
  , choiceTree
  , escape
  , camelWords
  , withoutPossessive
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T

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

-- The training of the chapter on looking around
-- まわりを見る章のトレーニング

-- ANCHOR: camel
-- 🐫 parseHttpRequest into its words: cut where a small letter is just
--    behind and a capital just ahead. The cut takes no character, so
--    no letter is lost.
-- parseHttpRequest を言葉に切る：すぐ後ろが小文字で、すぐ前が大文字の
-- 所で切る。切り目は文字を食べないので、どの文字もなくならない
camelWords :: Text -> [Text]
camelWords = split cut
  where
    cut = either (error . show) id (compile "(?<=\\p{Ll})(?=\\p{Lu})")
-- ANCHOR_END: camel

-- ANCHOR: possessive
-- 🔒 a*+ into (?>a*): every possessive quantifier, as a greedy one in
--    an atomic group. Look into groups, too.
-- a*+ を (?>a*) に：所有的な量指定子を、どれもアトミックグループの中の
-- 欲ばりにする。グループの中も見る
withoutPossessive :: Pattern -> Pattern
withoutPossessive (Pattern bs) = Pattern (fmap branch bs)
  where
    branch (Branch ps) = Branch (map piece ps)
    piece (Quantified a (Quantifier c Possessive)) =
      Quantified (Atomic (alone (Quantified (atom a) (Quantifier c Greedy))))
                 once
    piece (Quantified a q) = Quantified (atom a) q
    piece other            = other
    atom a = case a of
      Group k p  -> Group k (withoutPossessive p)
      Atomic p   -> Atomic (withoutPossessive p)
      Look d s p -> Look d s (withoutPossessive p)
      _          -> a
    alone p = Pattern (Branch [p] :| [])
-- ANCHOR_END: possessive
-- ANCHOR_END: all
