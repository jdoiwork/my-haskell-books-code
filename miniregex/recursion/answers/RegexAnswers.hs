-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE MultilineStrings #-}
-- | The answers to the training: the design chapter's, and then the
--   chapter on recursion's.
-- トレーニングの解答：設計の章のものと、再帰の章のもの
module RegexAnswers
  ( colourTree
  , choiceTree
  , escape
  , parensUpTo
  , balanced
  , isBalanced
  , balancedByHand
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
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

-- The training of the chapter on recursion
-- 再帰の章のトレーニング

-- ANCHOR: upto
-- 🪜 Notes nested up to k deep, with no recursion: the inside of each
--    note is written out again, k times. Depth 0 is a note with no note
--    in it.
-- k 段までの入れ子の注記を、再帰なしで：注記の中身を、k 回書き足す。
-- 0 段は、中に注記のない注記
parensUpTo :: Int -> Text
parensUpTo k
  | k <= 0    = "\\([^()]*\\)"
  | otherwise = "\\((?:[^()]|" <> parensUpTo (k - 1) <> ")*\\)"
-- ANCHOR_END: upto

-- ANCHOR: balanced
-- ⚖️ The three kinds of brackets, nested in the right way: the parts
--    are named in (?(DEFINE) ...), and call each other.
-- 3種類のかっこが、正しく入れ子になっている：部品に (?(DEFINE) ...) で
-- 名前を付け、おたがいに呼び合う
balanced :: Regex
balanced = case compile pattern of
  Right re -> re
  Left e   -> error (show e)
  where
    pattern = """
      (?x)
      (?(DEFINE)
        (?<inside> (?: [^()\\[\\]{}]++ | (?&pair) )* )
        (?<pair>   \\( (?&inside) \\)
                 | \\[ (?&inside) \\]
                 | \\{ (?&inside) \\} )
      )
      \\A (?&inside) \\z
      """

isBalanced :: Text -> Bool
isBalanced = isJust . find balanced

-- 📚 The same by hand, with a stack: push each opening bracket, and
--    pop its partner at each closing one.
-- 同じことを手で。スタックを使う：開きかっこを積み、閉じかっこごとに、
-- 相手を下ろす
balancedByHand :: Text -> Bool
balancedByHand = go [] . T.unpack
  where
    go stack [] = null stack
    go stack (c : cs)
      | c `elem` ("([{" :: String) = go (c : stack) cs
      | Just open <- lookup c pairs = case stack of
          top : rest | top == open -> go rest cs
          _                        -> False
      | otherwise = go stack cs
    pairs = [(')', '('), (']', '['), ('}', '{')]
-- ANCHOR_END: balanced
-- ANCHOR_END: all
