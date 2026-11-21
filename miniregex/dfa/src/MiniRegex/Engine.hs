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
-- からは ST の VM）を動かす。手数には上限があり、いつまでも続けずに、
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
  , searchAtBy
  , searchAt
  , searchWithinAt
  , nonEmptyAtBy
  , nonEmptyAtCursor
  , nonEmptyWithinAt
  , Runner
  , searchFromList
  , searchFromCont
  , searchWith
  , nonEmptyAt
  , nonEmptyWithin
  , nonEmptyWithinBy
  , chooseEngine
  , runnerFor
  , scanSwitching
  , scanShortFirst
  , budgetFor
  , toMatch
  ) where

-- ANCHOR: imports
import Control.Exception (throw)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Unsafe (lengthWord8)

import MiniRegex.Bounded (boundedFits, canBound, scanBounded)
import MiniRegex.Candidates (ends)
import MiniRegex.Caps
import MiniRegex.Compile (compileTree, showInst)
import MiniRegex.Continuation (matchAt)
import MiniRegex.Cursor hiding (before, rest)
import MiniRegex.Groups (groupCount, groupNames)
import MiniRegex.Parser (parsePattern)
import MiniRegex.Pike (blocker)
import MiniRegex.PikeST (scanPikeST)
import MiniRegex.Program (Program, instructions)
import MiniRegex.Prefilter (Prefilter (..), Start (..), places)
import MiniRegex.STVM (scanVMST, scanVMSTUntil)
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

-- ANCHOR: choose
-- 🚂 Choose the engine for a program. If the Pike VM can run it all,
--    Bounded (from the chapter on measuring): the backtracking VM that
--    remembers where it has been, for a short text; switching, for a
--    long one. If not, the backtracking VM, or, with Linear, an error
--    that names the instruction it cannot run.
-- プログラムのエンジンを選ぶ。Pike VM が全部動かせるなら、Bounded（測って
-- 比べる章から）：短い文字列には、通った所を覚えておくバックトラックの VM、
-- 長い文字列には乗り換え。だめなら、バックトラックの VM。Linear なら、
-- 動かせない命令を言うエラー
chooseEngine :: [Option] -> Program -> Either RegexError Engine
chooseEngine opts prog = case blocker prog of
  Nothing -> Right Bounded
  Just inst
    | Linear `elem` opts -> Left (NotLinear (showInst inst))
    | otherwise          -> Right Backtracking

-- 🏃 The VM of the engine the regex chose.
-- 正規表現が選んだエンジンの VM
runnerFor :: Regex -> Runner
runnerFor re
  -- from the chapter on learning from other lands: only texts, no VM
  -- よその国に学ぶ章から：文字列だけなら、VM を使わない
  | Literals ws <- start (prefilter re) = literalRunner ws
  | otherwise = case engine re of
      Switching    -> scanSwitching
      PikeVM       -> scanPikeST
      Backtracking -> scanVMST
      Bounded      -> scanShortFirst
-- ANCHOR_END: choose

-- ANCHOR: literalrunner
-- 📜 No VM at all, as Rust's regex does for a pattern that is only
--    texts: at each place, the first of the texts (in the order of the
--    branches) that is there. Each place costs one step.
-- VM を使わない。文字列だけのパターンに、Rust の regex がするのと同じ。
-- 場所ごとに、そこにある最初の文字列（枝の順）。1つの場所で1手
literalRunner :: [Text] -> Runner
literalRunner ws _ limit accept = go limit
  where
    go left []         = (Nothing, NotFound, left)
    go left _ | left <= 0 = (Nothing, OutOfSteps, 0)
    go left (c : cs)   =
      case [e | w <- ws, Just e <- [stepOver w c], accept e] of
        e : _ -> (Just c, Found e noCaps, left - 1)
        []    -> go (left - 1) cs
-- ANCHOR_END: literalrunner

-- ANCHOR: shortfirst
-- 📏 Like Rust's regex: the backtracking VM that remembers where it
--    has been, if its table fits the text (and the program has no
--    marks); switching, if not.
-- Rust の regex と同じ考え方：表が文字列に収まれば（そしてプログラムに
-- 印がなければ）、通った所を覚えておくバックトラックの VM。収まらなければ
-- 乗り換え
scanShortFirst :: Runner
scanShortFirst prog limit accept tries = case tries of
  first : _ | canBound prog, boundedFits prog first ->
    scanBounded prog limit accept tries
  _ -> scanSwitching prog limit accept tries
-- ANCHOR_END: shortfirst

-- ANCHOR: switching
-- 🔀 Backtracking first, the Pike VM if it takes too long. The
--    backtracking VM gets as many steps as the Pike VM could take at
--    worst (budgetFor); if they run out, the Pike VM goes on from the
--    place where they ran out (no match starts before it), with the
--    steps that are left. Most searches end within the budget, at the
--    backtracking VM's speed; the others cost at most about twice the
--    Pike VM's worst, so still linear time.
-- まずバックトラック、時間がかかりすぎたら Pike VM。バックトラックの VM
-- には、Pike VM が最悪でもかかる手数（budgetFor）だけ渡す。尽きたら、
-- 尽きた場所から（そこより前に始まる一致はない）、Pike VM が残りの手数で
-- 続ける。たいていの探しものは予算のうちに、バックトラックの VM の速さで
-- 終わる。そうでないものも、Pike VM の最悪の2倍ほどで済むので、やはり
-- 線形時間
scanSwitching :: Runner
scanSwitching prog limit accept tries = case tries of
  [] -> (Nothing, NotFound, limit)
  first : _
    | budget >= limit -> scanVMST prog limit accept tries
    | otherwise -> case scanVMSTUntil prog budget accept tries of
        Left rest -> scanPikeST prog (limit - budget) accept rest
        Right (o, answer, left) -> (o, answer, limit - budget + left)
    where
      budget = budgetFor prog first

-- 💰 The steps the Pike VM could take at worst from this place: each
--    instruction once at each place, twice over (once to follow, once
--    to move on). Bytes are counted, not characters: no need to count
--    from the head, and there are never fewer bytes than characters.
-- この場所から、Pike VM が最悪でもかかる手数：どの命令も、場所ごとに
-- 1度ずつ、それを2回分（たどるのと、進めるのと）。数えるのは文字では
-- なくバイト：頭から数えずに済み、バイトが文字より少ないことはない
budgetFor :: Program -> Cursor -> Int
budgetFor prog c =
  2 * length (instructions prog) * (lengthWord8 (whole c) - byte c + 1)
-- ANCHOR_END: switching

-- ANCHOR: search
-- 🏃 A VM that tries the places in turn: the pure one (scanVM) or the
--    one with ST (scanVMST). Both have this type, so the engine can run
--    either.
-- 場所を1つずつ試す VM：純粋なもの（scanVM）か、ST のもの
-- （scanVMST）。どちらもこの型なので、エンジンはどちらでも動かせる
type Runner
  =  Program -> Int -> (Cursor -> Bool) -> [Cursor]
  -> (Maybe Cursor, Answer, Int)

-- ANCHOR: counting
-- 🔦 The first match that starts at `from` or later, with a VM. All the
--    places share one limit on the steps, as PCRE2's match limit does.
--    Gives back the steps used, too.
-- from の位置か、それより後ろで始まる、最初の一致を VM で。どの場所も、
-- 1つの手数の上限を分け合う（PCRE2 の手数の上限と同じ）。使った手数も
-- 返す
searchCountingBy
  :: Runner -> Int -> Regex -> Text -> Int
  -> (Either GaveUp (Maybe Match), Int)
searchCountingBy vm limit re subject from = case cursorAt subject from of
  Nothing    -> (Right Nothing, 0)
  Just start -> (fmap (fmap fst) answer, used)
    where
      (answer, used) = searchAtBy vm limit re start

-- 🔖 The same, from a cursor: no counting from the head. Gives back
--    the cursor where the match ended, too (findAll goes on from it).
-- 同じものを、しおりから：頭から数えない。一致が終わった場所の
-- しおりも返す（findAll は、そこから続ける）
searchAtBy
  :: Runner -> Int -> Regex -> Cursor
  -> (Either GaveUp (Maybe (Match, Cursor)), Int)
searchAtBy vm limit re start =
  case vm (program re) limit (const True) tries of
    (Just c, Found end caps, left) ->
      (Right (Just (toMatch re c end caps, end)), limit - left)
    (_, OutOfSteps, _)    -> (Left (GaveUp limit), limit)
    (_, Looped g p, left) -> (Left (Looping g p), limit - left)
    (_, _, left)          -> (Right Nothing, limit - left)
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
-- 同じもの。手数が尽きたら Left
searchWithin :: Int -> Regex -> Text -> Int -> Either GaveUp (Maybe Match)
searchWithin limit re subject from =
  fst (searchCounting limit re subject from)

-- 🔦 With the default limit; GaveUp is thrown when it runs out.
-- 既定の上限で。尽きたら GaveUp を投げる
searchFrom :: Regex -> Text -> Int -> Maybe Match
searchFrom re subject from =
  either throw id (searchWithin defaultLimit re subject from)

-- 🔖 From a cursor, with the engine the regex chose: Left when the
--    steps run out, or (searchAt) GaveUp thrown.
-- しおりから、正規表現が選んだエンジンで。手数が尽きたら Left、
-- searchAt なら GaveUp を投げる
searchWithinAt :: Int -> Regex -> Cursor -> Either GaveUp (Maybe (Match, Cursor))
searchWithinAt limit re = fst . searchAtBy (runnerFor re) limit re

searchAt :: Regex -> Cursor -> Maybe (Match, Cursor)
searchAt re = either throw id . searchWithinAt defaultLimit re
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
nonEmptyWithinBy
  :: Runner -> Int -> Regex -> Text -> Int -> Either GaveUp (Maybe Match)
nonEmptyWithinBy vm limit re subject from = case cursorAt subject from of
  Nothing    -> Right Nothing
  Just start -> fmap (fmap fst) (nonEmptyAtBy vm limit re start)

-- 🔖 The same, from a cursor; with the cursor where the match ended.
-- 同じものを、しおりから。一致が終わった場所のしおりも
nonEmptyAtBy
  :: Runner -> Int -> Regex -> Cursor
  -> Either GaveUp (Maybe (Match, Cursor))
nonEmptyAtBy vm limit re start =
  case vm (program re) limit (\c -> at c /= at start) [start] of
    (_, Found end caps, _) -> Right (Just (toMatch re start end caps, end))
    (_, OutOfSteps, _)     -> Left (GaveUp limit)
    (_, Looped g p, _)     -> Left (Looping g p)
    (_, NotFound, _)       -> Right Nothing

nonEmptyWithin :: Int -> Regex -> Text -> Int -> Either GaveUp (Maybe Match)
nonEmptyWithin limit re = nonEmptyWithinBy (runnerFor re) limit re

nonEmptyAt :: Regex -> Text -> Int -> Maybe Match
nonEmptyAt re subject from =
  either throw id (nonEmptyWithin defaultLimit re subject from)

nonEmptyWithinAt
  :: Int -> Regex -> Cursor -> Either GaveUp (Maybe (Match, Cursor))
nonEmptyWithinAt limit re = nonEmptyAtBy (runnerFor re) limit re

nonEmptyAtCursor :: Regex -> Cursor -> Maybe (Match, Cursor)
nonEmptyAtCursor re = either throw id . nonEmptyWithinAt defaultLimit re
-- ANCHOR_END: nonempty
-- ANCHOR_END: all
