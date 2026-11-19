-- ANCHOR: all
-- | The engine. compilePattern reads with the parser; searchFrom runs
--   the backtracking matcher from each place in turn, and gives back
--   what the groups found, too. nonEmptyAt is for findAll, after an
--   empty match. Only two things are still stopped at the door: the
--   flag i and the possessive quantifier (for later chapters).
-- エンジン。compilePattern はパーサーで読む。searchFrom は、バック
-- トラックのマッチャーを、場所を1つずつずらしながら動かし、グループが
-- 見つけたものも返す。nonEmptyAt は、空の一致のあとの findAll のため。
-- 入り口で止めるのは、あと2つだけ：フラグ i と、所有的な量指定子
-- （あとの章で）
module MiniRegex.Engine
  ( compilePattern
  , searchFrom
  , searchFromList
  , searchWith
  , nonEmptyAt
  ) where

-- ANCHOR: imports
import Data.Maybe (listToMaybe, mapMaybe)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text (Text)

import MiniRegex.Candidates (ends)
import MiniRegex.Caps
import MiniRegex.Continuation (matchAt, run)
import MiniRegex.Cursor
import MiniRegex.Groups (groupCount, groupNames)
import MiniRegex.Parser (parsePattern)
import MiniRegex.Syntax
import MiniRegex.Types
-- ANCHOR_END: imports

-- ANCHOR: compile
-- 🏗️ Read a pattern into a tree, with the parser; then stop the two
--    things the matcher cannot run yet.
-- パーサーでパターンを読んで木にする。そのあと、マッチャーがまだ
-- 動かせない2つだけを止める
compilePattern :: [Option] -> Text -> Either RegexError Pattern
compilePattern opts src = do
  tree <- parsePattern opts src
  matcherCanRun opts tree
  pure tree

-- 🚧 The flag i waits for the chapter on Unicode, the possessive
--    quantifier (a*+) for the chapter on looking around.
-- フラグ i は Unicode の章を、所有的な量指定子（a*+）は、まわりを
-- 見る章を待つ
matcherCanRun :: [Option] -> Pattern -> Either RegexError ()
matcherCanRun opts tree
  | Caseless `elem` opts = Left (Unsupported 0 "matcher: flag i")
  | hasPossessive tree   = Left (Unsupported 0 "matcher: possessive")
  | otherwise            = Right ()

-- 🔎 Is there a possessive quantifier anywhere in the tree?
-- 木のどこかに、所有的な量指定子があるか
hasPossessive :: Pattern -> Bool
hasPossessive (Pattern branches) = any inBranch (NE.toList branches)
  where
    inBranch (Branch ps) = any inPiece ps
    inPiece (Anchor _) = False
    inPiece (Quantified a q) = greed q == Possessive || inAtom a
    inAtom (Group _ p) = hasPossessive p
    inAtom _           = False
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
  :: ([Option] -> Pattern -> Cursor -> Maybe (Cursor, Caps))
  -> Regex -> Text -> Int -> Maybe Match
searchWith matcher re subject from = do
  start <- cursorAt subject from
  listToMaybe (mapMaybe tryAt (onwards start))
  where
    tryAt c = do
      (end, caps) <- matcher (options re) (tree re) c
      pure (toMatch re c end caps)

-- 📦 A Match: the text between the two cursors, and the groups.
-- 見つけたもの：2つのしおりのあいだの文字列と、グループ
toMatch :: Regex -> Cursor -> Cursor -> Caps -> Match
toMatch re from to caps = Match
  { matchText   = taken from to
  , matchStart  = at from
  , matchEnd    = at to
  , matchGroups = groupList (groupCount (tree re)) caps
  , matchNames  = Map.fromList (groupNames (tree re))
  }

-- 📋 With the list of candidates: the first one.
-- 候補のリストで：先頭の1つ
searchFromList :: Regex -> Text -> Int -> Maybe Match
searchFromList = searchWith (\opts p c -> listToMaybe (ends opts p c))

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
  (end, caps) <- run (options re) (tree re) notHere start noCaps
  pure (toMatch re start end caps)
  where
    notHere c caps
      | at c == from = Nothing
      | otherwise    = Just (c, caps)
-- ANCHOR_END: nonempty
-- ANCHOR_END: all
