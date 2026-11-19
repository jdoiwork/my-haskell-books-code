-- ANCHOR: all
-- | The engine. compilePattern reads with the parser; compileTree
--   turns the tree into a program for the VM. searchFrom runs the VM
--   from each place in turn, with a limit on the steps: it gives up
--   (throws GaveUp) rather than go on for ever. searchWithin says so
--   with Left instead. The list and continuation matchers stay, to
--   compare with.
-- エンジン。compilePattern はパーサーで読み、compileTree が木を VM の
-- プログラムにする。searchFrom は、場所を1つずつずらしながら VM を
-- 動かす。手数には上限があり、いつまでも続けずに、あきらめる（GaveUp を
-- 投げる）。searchWithin は、それを Left で知らせる。候補のリストと継続の
-- マッチャーは、くらべる相手として残す
module MiniRegex.Engine
  ( compilePattern
  , compileTree
  , searchFrom
  , searchWithin
  , searchCounting
  , searchFromList
  , searchFromCont
  , searchWith
  , nonEmptyAt
  , nonEmptyWithin
  ) where

-- ANCHOR: imports
import Control.Exception (throw)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex.Candidates (ends)
import MiniRegex.Caps
import MiniRegex.Compile (compileTree)
import MiniRegex.Continuation (matchAt)
import MiniRegex.Cursor
import MiniRegex.Groups (groupCount, groupNames)
import MiniRegex.Parser (parsePattern)
import MiniRegex.Syntax (Pattern)
import MiniRegex.Types
import MiniRegex.VM
-- ANCHOR_END: imports

-- ANCHOR: compile
-- 🏗️ Read a pattern into a tree, with the parser (the flags go into
--    the tree). The program is made from the tree, by compileTree.
-- パーサーでパターンを読んで木にする（フラグも木に入る）。プログラムは、
-- その木から compileTree が作る
compilePattern :: [Option] -> Text -> Either RegexError Pattern
compilePattern = parsePattern
-- ANCHOR_END: compile

-- ANCHOR: search
-- 🔦 The first match that starts at `from` or later, with the VM: try
--    each place from the left. All the places share one limit on the
--    steps, as PCRE2's match limit does. Gives back the steps used, too.
-- from の位置か、それより後ろで始まる、最初の一致を VM で。左から
-- 1つずつ、その場所で試す。どの場所も、1つの手数の上限を分け合う
-- （PCRE2 の手数の上限と同じ）。使った手数も返す
searchCounting
  :: Int -> Regex -> Text -> Int -> (Either GaveUp (Maybe Match), Int)
searchCounting limit re subject from = case cursorAt subject from of
  Nothing    -> (Right Nothing, 0)
  Just start -> go limit (onwards start)
  where
    go n [] = (Right Nothing, used n)
    go n (c : cs) = case runVM (program re) n (const True) c of
      (Found end caps, left) -> (Right (Just (toMatch re c end caps)), used left)
      (NotFound, left)       -> go left cs
      (OutOfSteps, _)        -> (Left (GaveUp limit), limit)
    used left = limit - left

-- 📮 The same, with Left when the steps ran out.
-- 同じもの。手数が尽きたら Left
searchWithin :: Int -> Regex -> Text -> Int -> Either GaveUp (Maybe Match)
searchWithin limit re subject from =
  fst (searchCounting limit re subject from)

-- 🔦 With the default limit; GaveUp is thrown when it runs out.
-- 既定の上限で。尽きたら GaveUp を投げる
searchFrom :: Regex -> Text -> Int -> Maybe Match
searchFrom re subject from =
  either throw id (searchWithin defaultLimit re subject from)
-- ANCHOR_END: search

-- ANCHOR: tomatch
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
-- ANCHOR_END: tomatch

-- ANCHOR: others
-- 🧪 The tree-walking matchers of the chapters before, to compare with.
--    They take no limit.
-- 前の章までの、木を歩くマッチャー。くらべる相手。手数の上限はない
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

-- 📋 With the list of candidates: the first one.
-- 候補のリストで：先頭の1つ
searchFromList :: Regex -> Text -> Int -> Maybe Match
searchFromList = searchWith (\p c -> listToMaybe (ends p c))

-- 📮 With continuations.
-- 継続で
searchFromCont :: Regex -> Text -> Int -> Maybe Match
searchFromCont = searchWith matchAt
-- ANCHOR_END: others

-- ANCHOR: nonempty
-- 🚫 A match that starts just here and is not empty (or Nothing). The
--    VM is the same; only its `accept` says no to the place where the
--    match started.
-- ちょうどここで始まる、空でない一致（なければ Nothing）。VM は同じで、
-- accept が、一致が始まった場所に「いいえ」と言うだけ
nonEmptyWithin :: Int -> Regex -> Text -> Int -> Either GaveUp (Maybe Match)
nonEmptyWithin limit re subject from = case cursorAt subject from of
  Nothing    -> Right Nothing
  Just start -> case runVM (program re) limit (\c -> at c /= from) start of
    (Found end caps, _) -> Right (Just (toMatch re start end caps))
    (NotFound, _)       -> Right Nothing
    (OutOfSteps, _)     -> Left (GaveUp limit)

nonEmptyAt :: Regex -> Text -> Int -> Maybe Match
nonEmptyAt re subject from =
  either throw id (nonEmptyWithin defaultLimit re subject from)
-- ANCHOR_END: nonempty
-- ANCHOR_END: all
