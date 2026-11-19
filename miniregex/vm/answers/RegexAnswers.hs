-- ANCHOR: all
-- ANCHOR: head
-- | The answers to the training: the design chapter's, and then the
--   chapter on the VM's.
-- トレーニングの解答：設計の章のものと、命令の列の章のもの
module RegexAnswers
  ( colourTree
  , choiceTree
  , escape
  , findIO
  , smallestLimit
  ) where

import Control.Exception (evaluate, try)
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

-- The training of the chapter on the VM
-- 命令の列の章のトレーニング

-- ANCHOR: findio
-- 🧯 find, but a GaveUp that it throws is caught and given back as a
--    Left. evaluate makes find run here, inside try: without it, try
--    would only get a thunk, and the GaveUp would come out later.
-- find と同じ。ただし、投げた GaveUp を受け止めて、Left で返す。
-- evaluate が、ここで、try の中で find を動かす。なければ、try が受け取る
-- のはサンクだけで、GaveUp はあとで飛び出してくる
findIO :: Regex -> Text -> IO (Either GaveUp (Maybe Match))
findIO re t = try (evaluate (find re t))
-- ANCHOR_END: findio

-- ANCHOR: smallest
-- 🔍 The smallest limit with which findWithin answers (the way
--    pcre2test's find_limits does): halve the range each time. Nothing
--    when even the default limit is not enough.
-- findWithin が答えを出せる、いちばん小さい上限（pcre2test の
-- find_limits と同じ）。範囲を毎回半分にする。既定の上限でも足りなければ
-- Nothing
smallestLimit :: Regex -> Text -> Maybe Int
smallestLimit re t
  | answers defaultLimit = Just (go 0 defaultLimit)
  | otherwise            = Nothing
  where
    answers limit = either (const False) (const True) (findWithin limit re t)
    -- not enough at lo, enough at hi
    -- lo では足りない、hi なら足りる
    go lo hi
      | hi - lo <= 1 = hi
      | answers mid  = go lo mid
      | otherwise    = go mid hi
      where
        mid = (lo + hi) `div` 2
-- ANCHOR_END: smallest
-- ANCHOR_END: all
