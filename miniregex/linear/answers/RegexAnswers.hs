-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE MultilineStrings #-}
-- | The answers to the training: the design chapter's, and then the
--   linear-time chapter's.
-- トレーニングの解答：設計の章のものと、線形時間の章のもの
module RegexAnswers
  ( colourTree
  , choiceTree
  , escape
  , searchBox
  , matchesBySet
  ) where

import Data.Array ((!))
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex
import MiniRegex.Cursor (cursorAt, step)
import MiniRegex.Program (Inst (Assert, IfStill, Jmp, Split, Take))
import MiniRegex.Program qualified as Program
import MiniRegex.Rules (fitsChar, holds)
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

-- The training of the chapter on recursion
-- 再帰の章のトレーニング

-- ANCHOR: box
-- 🔎 The search box of the records room: a pattern from someone else
--    runs only if it runs in linear time. If not, the box says why.
-- 記録室の検索の欄：ほかの人のパターンは、線形時間で動くときだけ
-- 動かす。だめなら、欄がわけを言う
searchBox :: Text -> Text -> Either Text [Text]
searchBox p t = case compileWith [Linear] p of
  Left err -> Left (explain p err)
  Right re -> Right (map matchText (findAll re t))
-- ANCHOR_END: box

-- ANCHOR: byset
-- 🫧 Is the pattern anywhere in the text? Thompson's way, with no
--    groups: only the set of the places in the program where threads
--    wait. No marks either: an ifstill may go both ways (going round a
--    repeat that took nothing finds nothing new).
-- パターンは文字列のどこかにあるか。Thompson のやり方で、グループなしで：
-- スレッドが待っている、プログラムの場所の集合だけ。印も持たない。
-- ifstill は両方へ行ってよい（何も食べなかったくり返しを、もう一度
-- 回っても、新しいものは見つからない）
matchesBySet :: Regex -> Text -> Bool
matchesBySet re t = maybe False (go IntSet.empty) (cursorAt t 0)
  where
    prog = program re
    -- the threads that were there, and a new one for a match from here
    -- いたスレッドと、ここから始まる一致のための新しいスレッド
    go now c
      | any (\pc -> prog ! pc == Program.Match) waiting = True
      | otherwise = case step c of
          Nothing      -> False
          Just (x, c') -> go (IntSet.fromList (moved x)) c'
      where
        waiting = IntSet.toList (close c (0 : IntSet.toList now))
        moved x = [ pc + 1 | pc <- waiting, Take a <- [prog ! pc], fitsChar a x ]
    -- follow the instructions that take no character; keep the places
    -- that wait (a take or a match)
    -- 文字を食べない命令をたどる。待つ場所（take か match）だけを残す
    close c starts = IntSet.filter waits (foldl (add c) IntSet.empty starts)
    waits pc = case prog ! pc of
      Take _        -> True
      Program.Match -> True
      _             -> False
    add c seen pc
      | IntSet.member pc seen = seen
      | otherwise = case prog ! pc of
          Split a b     -> add c (add c seen' a) b
          Jmp a         -> add c seen' a
          IfStill _ a   -> add c (add c seen' (pc + 1)) a
          Assert a
            | holds a c   -> add c seen' (pc + 1)
            | otherwise   -> seen'
          Take _        -> seen'
          Program.Match -> seen'
          -- open, close, keep, mark: nothing to do without groups
          -- open・close・keep・mark：グループがなければ、することはない
          _             -> add c seen' (pc + 1)
      where
        seen' = IntSet.insert pc seen
-- ANCHOR_END: byset
-- ANCHOR_END: all
