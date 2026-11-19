-- ANCHOR: all
-- ANCHOR: head
-- | The answers to the training: the design chapter's, and then the
--   chapter on measuring.
-- トレーニングの解答：設計の章のものと、測って比べる章のもの
module RegexAnswers
  ( colourTree
  , choiceTree
  , escape
  , maxBoundedBytes
  , findAllLines
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex
import MiniRegex.Bounded (visitedCapacity)
import MiniRegex.Program (instructions)
import MiniRegex.Syntax
import MiniRegex.Types (program)
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

-- The chapter on measuring
-- 測って比べる章

-- ANCHOR: longest
-- 📏 The longest text (in bytes) that the table of the bounded
--    backtracker takes for this regex: one bit for each instruction,
--    at each byte and after the last one.
-- 通った所を覚えるバックトラックの表が、この正規表現で受け取れる、
-- いちばん長い文字列（バイト）。命令ごとに、どのバイトにも、最後の
-- バイトのうしろにも1ビット
maxBoundedBytes :: Regex -> Int
maxBoundedBytes re = visitedCapacity `div` size - 1
  where
    size = length (instructions (program re))
-- ANCHOR_END: longest

-- ANCHOR: lines
-- 📃 findAll, one line at a time, with the places moved back to the
--    whole text. Only for a pattern that never takes a newline.
-- findAll を1行ずつ。場所は、文字列全体の場所に直す。改行を食べない
-- パターンだけ
findAllLines :: Regex -> Text -> [Match]
findAllLines re t = concat (zipWith inLine starts ls)
  where
    ls     = T.splitOn "\n" t
    starts = scanl (\at l -> at + T.length l + 1) 0 ls
    inLine at l = map (shift at) (findAll re l)
    shift at m = m
      { matchStart  = matchStart m + at
      , matchEnd    = matchEnd m + at
      , matchGroups = map (fmap (move at)) (matchGroups m)
      }
    move at sp = sp { spanStart = spanStart sp + at
                    , spanEnd   = spanEnd sp + at }
-- ANCHOR_END: lines
-- ANCHOR_END: all
