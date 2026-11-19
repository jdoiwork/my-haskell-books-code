-- ANCHOR: all
-- | The engine. compilePattern reads with the parser; searchFrom runs
--   the backtracking matcher from each place in turn, and gives back
--   what the groups found, too. nonEmptyAt is for findAll, after an
--   empty match. With \K, the match that is told starts where it was.
-- エンジン。compilePattern はパーサーで読む。searchFrom は、バック
-- トラックのマッチャーを、場所を1つずつずらしながら動かし、グループが
-- 見つけたものも返す。nonEmptyAt は、空の一致のあとの findAll のため。
-- \K があれば、知らせる一致の始まりは、その場所
module MiniRegex.Engine
  ( compilePattern
  , searchFrom
  , searchFromList
  , searchWith
  , nonEmptyAt
  ) where

-- ANCHOR: imports
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex.Candidates (ends)
import MiniRegex.Caps
import MiniRegex.Continuation (matchAt, run)
import MiniRegex.Cursor
import MiniRegex.Groups (groupCount, groupNames)
import MiniRegex.Parser (parsePattern)
import MiniRegex.Syntax (Pattern)
import MiniRegex.Types
-- ANCHOR_END: imports

-- ANCHOR: compile
-- 🏗️ Read a pattern into a tree, with the parser (the flags go into
--    the tree). The matcher can run every tree now: nothing is
--    stopped at the door any more.
-- パーサーでパターンを読んで木にする（フラグも木に入る）。マッチャーは、
-- もう、どの木でも動かせる。入り口で止めるものは、なくなった
compilePattern :: [Option] -> Text -> Either RegexError Pattern
compilePattern = parsePattern
-- ANCHOR_END: compile

-- ANCHOR: search
-- 🔦 The first match that starts at `from` or later: try the matcher
--    at each place, from the left, and take the first place where it
--    works. `from` counts characters of the whole text, so that ^ can
--    see what is before it.
-- from の位置か、それより後ろで始まる、最初の一致。左から1つずつ、
-- その場所でマッチャーを試し、うまくいった最初の場所を取る。from は
-- 文字列全体の何文字目かで数える（^ が、その手前を見られるように）
searchWith
  :: (Pattern -> Cursor -> Maybe (Cursor, Caps))
  -> Regex -> Text -> Int -> Maybe Match
searchWith matcher re subject from = do
  start <- cursorAt subject from
  listToMaybe (mapMaybe tryAt (onwards start))
  where
    tryAt c = do
      (end, caps) <- matcher (tree re) c
      pure (toMatch re c end caps)

-- 📦 A Match: the text between the two cursors (or from the last \K),
--    and the groups.
-- 見つけたもの：2つのしおりのあいだ（\K があれば、最後の \K から）の
-- 文字列と、グループ
toMatch :: Regex -> Cursor -> Cursor -> Caps -> Match
toMatch re from to caps = Match
  { matchText   = T.drop (start - at from) (taken from to)
  , matchStart  = start
  , matchEnd    = at to
  , matchGroups = groupList (groupCount (tree re)) caps
  , matchNames  = Map.fromList (groupNames (tree re))
  }
  where
    start = fromMaybe (at from) (keptFrom caps)

-- 📋 With the list of candidates: the first one.
-- 候補のリストで：先頭の1つ
searchFromList :: Regex -> Text -> Int -> Maybe Match
searchFromList = searchWith (\p c -> listToMaybe (ends p c))

-- 📮 With continuations: the one the engine uses.
-- 継続で：エンジンが使うのは、こちら
searchFrom :: Regex -> Text -> Int -> Maybe Match
searchFrom = searchWith matchAt
-- ANCHOR_END: search

-- ANCHOR: nonempty
-- 🚫 A match that starts just here and is not empty (or Nothing). Only
--    the rest of the work at the very end is new: it says no to the
--    place where the match started.
-- ちょうどここで始まる、空でない一致（なければ Nothing）。新しいのは、
-- いちばん最後の残りの仕事だけ：一致が始まった場所なら「いいえ」と言う
nonEmptyAt :: Regex -> Text -> Int -> Maybe Match
nonEmptyAt re subject from = do
  start <- cursorAt subject from
  (end, caps) <- run (tree re) notHere start noCaps
  pure (toMatch re start end caps)
  where
    notHere c caps
      | at c == from = Nothing
      | otherwise    = Just (c, caps)
-- ANCHOR_END: nonempty
-- ANCHOR_END: all
