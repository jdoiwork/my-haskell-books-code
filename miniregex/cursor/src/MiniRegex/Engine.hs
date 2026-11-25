-- ANCHOR: all
-- | The engine. compilePattern reads with the parser; compileTree
--   turns the tree into a program for the VM. searchFrom runs the VM
--   (the one with ST, from this chapter on) from each place in turn,
--   with a limit on the steps: it gives up (throws GaveUp) rather than
--   go on for ever. searchWithin says so with Left instead. The pure
--   VM, and the list and continuation matchers, stay, to compare with.
--   From the chapter on looking first, the VM is handed only the places
--   the prefilter lets through.
-- エンジン。compilePattern はパーサーで読み、compileTree が木を VM の
-- プログラムにする。searchFrom は、場所を1つずつずらしながら VM（この章
-- からは ST の VM）を動かす。ステップ数には上限があり、いつまでも続けずに、
-- あきらめる（GaveUp を投げる）。searchWithin は、それを Left で知らせる。
-- 純粋な VM と、候補のリストと継続のマッチャーは、くらべる相手として残す。
-- 先に探す章からは、VM には、先に見て通した場所だけを渡す
module MiniRegex.Engine
  ( compilePattern
  , compileTree
  , searchFrom
  , searchWithin
  , searchCounting
  , searchCountingBy
  , Runner
  , searchFromList
  , searchFromCont
  , searchWith
  , nonEmptyAt
  , nonEmptyWithin
  , nonEmptyWithinBy
  , chooseEngine
  , runnerFor
  ) where

-- ANCHOR: imports
import Control.Exception (throw)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex.Candidates (ends)
import MiniRegex.Caps
import MiniRegex.Compile (compileTree, programLimit, programSize, showInst)
import MiniRegex.Continuation (matchAt)
import MiniRegex.Cursor hiding (before, rest)
import MiniRegex.Groups (groupCount, groupNames)
import MiniRegex.Parser (parsePattern)
import MiniRegex.Pike (blocker, scanPike)
import MiniRegex.Prefilter (places)
import MiniRegex.Program (Program)
import MiniRegex.STVM (scanVMST)
import MiniRegex.Syntax (Pattern)
import MiniRegex.Types
import MiniRegex.VM
-- ANCHOR_END: imports

-- ANCHOR: compile
-- 🏗️ Read a pattern into a tree, with the parser (the flags go into
--    the tree). The program is made from the tree, by compileTree. A
--    tree whose program would be too big is "not yet": the limit on
--    the steps cannot stop a program that is still being made.
-- パーサーでパターンを読んで木にする（フラグも木に入る）。プログラムは、
-- その木から compileTree が作る。プログラムが大きくなりすぎる木は「まだ」：
-- ステップ数の上限では、作っている途中のプログラムは止められない
compilePattern :: [Option] -> Text -> Either RegexError Pattern
compilePattern opts src = do
  tree <- parsePattern opts src
  if programSize tree > programLimit
    then Left (Unsupported 0 "pattern too large for the VM")
    else Right tree
-- ANCHOR_END: compile

-- ANCHOR: choose
-- 🚂 Choose the engine for a program. If the Pike VM can run it all,
--    the Pike VM; if not, the backtracking VM, or, with Linear, an
--    error that names the instruction it cannot run.
-- プログラムのエンジンを選ぶ。Pike VM が全部動かせるなら Pike VM。
-- だめなら、バックトラックの VM。Linear なら、動かせない命令を言う
-- エラー
chooseEngine :: [Option] -> Program -> Either RegexError Engine
chooseEngine opts prog = case blocker prog of
  Nothing -> Right PikeVM
  Just inst
    | Linear `elem` opts -> Left (NotLinear (showInst inst))
    | otherwise          -> Right Backtracking

-- 🏃 The VM of the engine the regex chose.
-- 正規表現が選んだエンジンの VM
runnerFor :: Regex -> Runner
runnerFor re = case engine re of
  PikeVM       -> scanPike
  Backtracking -> scanVMST
-- ANCHOR_END: choose

-- ANCHOR: search
-- 🏃 A VM that tries the places in turn: the pure one (scanVM), the
--    one with ST (scanVMST), or the Pike VM (scanPike). All have this
--    type, so the engine can run any of them.
-- 場所を1つずつ試す VM：純粋なもの（scanVM）、ST のもの（scanVMST）、
-- Pike VM（scanPike）。どれもこの型なので、エンジンはどれでも動かせる
type Runner
  =  Program -> Int -> (Cursor -> Bool) -> [Cursor]
  -> (Maybe Cursor, Answer, Int)

-- ANCHOR: counting
-- 🔦 The first match that starts at `from` or later, with a VM. All the
--    places share one limit on the steps (PCRE2 starts its count again
--    at each place: MiniRegex's limit is for the whole search). Gives
--    back the steps used, too.
-- from の位置か、それより後ろで始まる、最初の一致を VM で。どの場所も、
-- 1つの上限を分け合う（PCRE2 は場所ごとに数え直す。MiniRegex の上限は、
-- 探し全体のもの）。使ったステップ数も返す
searchCountingBy
  :: Runner -> Int -> Regex -> Text -> Int
  -> (Either GaveUp (Maybe Match), Int)
searchCountingBy vm limit re subject from = case cursorAt subject from of
  Nothing    -> (Right Nothing, 0)
  Just start -> case vm (program re) limit (const True) tries of
    (Just c, Found end caps, left) ->
      (Right (Just (toMatch re c end caps)), limit - left)
    (_, OutOfSteps, _) -> (Left (GaveUp limit), limit)
    (_, Looped g p, left) -> (Left (Looping g p), limit - left)
    (_, _, left)       -> (Right Nothing, limit - left)
    where
      -- only the places the prefilter lets through
      -- 先に見て、通した場所だけ
      tries = places (prefilter re) start
-- ANCHOR_END: counting

-- 🔦 The engine the regex chose does the work: the Pike VM, or the
--    backtracking VM with ST.
-- 正規表現が選んだエンジンが働く：Pike VM か、ST のバックトラックの VM
searchCounting
  :: Int -> Regex -> Text -> Int -> (Either GaveUp (Maybe Match), Int)
searchCounting limit re = searchCountingBy (runnerFor re) limit re

-- 📮 The same, with Left when the steps ran out.
-- 同じもの。ステップ数が尽きたら Left
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
-- 前の章までの、木を歩くマッチャー。くらべる相手。ステップ数の上限はない
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
nonEmptyWithinBy
  :: Runner -> Int -> Regex -> Text -> Int -> Either GaveUp (Maybe Match)
nonEmptyWithinBy vm limit re subject from = case cursorAt subject from of
  Nothing    -> Right Nothing
  Just start -> case vm (program re) limit (\c -> at c /= from) [start] of
    (_, Found end caps, _) -> Right (Just (toMatch re start end caps))
    (_, OutOfSteps, _)     -> Left (GaveUp limit)
    (_, Looped g p, _)     -> Left (Looping g p)
    (_, NotFound, _)       -> Right Nothing

nonEmptyWithin :: Int -> Regex -> Text -> Int -> Either GaveUp (Maybe Match)
nonEmptyWithin limit re = nonEmptyWithinBy (runnerFor re) limit re

nonEmptyAt :: Regex -> Text -> Int -> Maybe Match
nonEmptyAt re subject from =
  either throw id (nonEmptyWithin defaultLimit re subject from)
-- ANCHOR_END: nonempty
-- ANCHOR_END: all
