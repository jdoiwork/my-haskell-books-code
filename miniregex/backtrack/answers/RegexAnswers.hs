-- ANCHOR: all
-- ANCHOR: head
-- | The answers to the training: the design chapter's, and then the
--   matcher chapter's.
-- トレーニングの解答：設計の章のものと、マッチャーの章のもの
module RegexAnswers
  ( colourTree
  , choiceTree
  , escape
  , findLongest
  , fullMatch
  ) where

import Data.List (maximumBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex.Candidates (ends)
import MiniRegex.Continuation (run)
import MiniRegex.Cursor
import MiniRegex.Syntax
import MiniRegex.Types
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

-- The matcher chapter's training
-- マッチャーの章のトレーニング

-- ANCHOR: longest
-- 📏 POSIX's way: from the leftmost place where the pattern can match,
--    take the longest of all the candidates, not the first one.
-- POSIX のやり方：一致できるいちばん左の場所で、最初の候補ではなく、
-- 候補の全部のうち、いちばん長いものを取る
findLongest :: Regex -> Text -> Maybe Match
findLongest re t = do
  start <- cursorAt t 0
  listToMaybe (mapMaybe longestAt (onwards start))
  where
    longestAt c = case ends (options re) (tree re) c of
      [] -> Nothing
      es -> let e = maximumBy (comparing at) es
            in  Just (Match (taken c e) (at c) (at e))
-- ANCHOR_END: longest

-- ANCHOR: full
-- 🧱 Does the pattern take the whole text? The rest of the work, at
--    the very end, is not "here" but "here, if the text is over".
-- パターンが文字列の全部に合うか。いちばん最後の残りの仕事を、「ここ」
-- ではなく、「文字列が終わっていれば、ここ」にする
fullMatch :: Regex -> Text -> Bool
fullMatch re t = isJust (run (options re) (tree re) atEnd start)
  where
    start = Cursor 0 Nothing t
    atEnd c
      | T.null (rest c) = Just c
      | otherwise       = Nothing
-- ANCHOR_END: full
-- ANCHOR_END: all
