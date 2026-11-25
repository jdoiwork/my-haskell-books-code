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
  , searchSharing
  , nonEmptySharing
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
import MiniRegex.Compile (compileTree, programLimit, programSize, showInst)
import MiniRegex.Continuation (matchAt)
import MiniRegex.Cursor hiding (before, rest)
import MiniRegex.Groups (groupCount, groupNames)
import MiniRegex.Parser (parsePattern)
import MiniRegex.Pike (blocker)
import MiniRegex.PikeST (scanPikeST)
import MiniRegex.Program (Program, instructions)
import MiniRegex.Prefilter (places)
import MiniRegex.STVM (scanVMST, scanVMSTUntil)
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
runnerFor re = case engine re of
  Switching    -> scanSwitching
  PikeVM       -> scanPikeST
  Backtracking -> scanVMST
  Bounded      -> scanShortFirst
-- ANCHOR_END: choose

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
--    backtracking VM gets a budget of steps (budgetFor); if they run
--    out, the Pike VM goes on from the place where they ran out (no
--    match starts before it), with the steps that are left. Most
--    searches end within the budget, at the backtracking VM's speed;
--    the others cost the budget and then the Pike VM's steps, and both
--    grow with the text only in proportion: still linear time.
-- まずバックトラック、時間がかかりすぎたら Pike VM。バックトラックの VM
-- には、予算のステップ数（budgetFor）だけ渡す。尽きたら、尽きた場所から
-- （そこより前に始まる一致はない）Pike VM が残りのステップ数で続ける。
-- たいていの探しものは予算のうちに、バックトラックの VM の速さで終わる。
-- そうでないものも、予算と Pike VM のステップ数の和で、どちらも文字列の
-- 長さに比例するだけなので、やはり線形時間
scanSwitching :: Runner
scanSwitching prog limit accept tries = case tries of
  [] -> (Nothing, NotFound, limit)
  first : _ -> case scanVMSTUntil prog budget accept tries of
    Left rest -> scanPikeST prog (limit - budget) accept rest
    Right (o, answer, left) -> (o, answer, limit - budget + left)
    where
      -- the budget is paid out of the limit: at most a quarter of it,
      -- so that the Pike VM always has three quarters left (on a long
      -- text, the whole budget would leave it almost nothing)
      -- 予算は上限の中から払う：多くても4分の1。Pike VM には、いつも
      -- 4分の3が残る（長い文字列で予算をまるごと払うと、ほとんど残らない）
      budget = min (budgetFor prog first) (limit `div` 4)

-- ANCHOR: sharing
-- 👛 Switching with a budget shared by every search of one walk
--    (findAll): the backtracking VM gets what is left of it (a quarter
--    of the limit at most, as above), and gives back what is left
--    after it. Once it is spent, every search goes straight to the
--    Pike VM. So the backtracking VM takes, in all the searches of the
--    walk, no more than one budget: with a new budget for each match, a
--    pattern that loses its way on each short line would cost the rest
--    of the text, at each match (from the chapter on switching engines,
--    Fable #35). (A pattern that makes the Pike VM read to the end at
--    each match, such as a.*b|a, is still twice as slow for twice the
--    text, and twice again.)
-- 1つの歩き（findAll）の、どの探しものにも同じ予算を分けて使う乗り換え。
-- バックトラックの VM は、その残りを受け取り（上と同じく、多くても上限の
-- 4分の1）、使ったあとの残りを返す。使い切ったら、どの探しものも、すぐ
-- Pike VM へ。だからバックトラックの VM が使うのは、歩きの全部の探しもので、
-- 予算1つぶんまで。一致のたびに新しい予算を渡すと、短い行ごとに迷うパターン
-- で、一致のたびに残りの文字列ぶんを使ってしまう（a.*b|a のように、Pike VM
-- が一致のたびに終わりまで読むパターンは、それでも長さの2乗）
scanSwitchingWith
  :: Int -> Program -> Int -> (Cursor -> Bool) -> [Cursor]
  -> ((Maybe Cursor, Answer, Int), Int)
scanSwitchingWith shared prog limit accept tries =
  case scanVMSTUntil prog budget accept tries of
    Left rest ->
      (scanPikeST prog (limit - budget) accept rest, shared - budget)
    Right (o, answer, left) ->
      ((o, answer, limit - budget + left), shared - budget + left)
  where
    budget = min shared (limit `div` 4)

-- 🔎 The first match from a place, and what is left of the shared
--    budget.
-- その場所からの最初の一致と、分け合う予算の残り
searchSharing
  :: Int -> Regex -> Cursor -> (Either GaveUp (Maybe (Match, Cursor)), Int)
searchSharing shared re start = (fst (searchAtBy runner limit re start), left)
  where
    limit = defaultLimit
    (answer, left) = scanSwitchingWith shared (program re) limit (const True)
                       (places (prefilter re) start)
    runner _ _ _ _ = answer

-- 🚫 A match just at this place that is not empty, the same way.
-- ちょうどこの場所の、空でない一致を、同じやり方で
nonEmptySharing
  :: Int -> Regex -> Cursor -> (Either GaveUp (Maybe (Match, Cursor)), Int)
nonEmptySharing shared re start = (nonEmptyAtBy runner limit re start, left)
  where
    limit = defaultLimit
    (answer, left) = scanSwitchingWith shared (program re) limit
                       (\c -> at c /= at start) [start]
    runner _ _ _ _ = answer
-- ANCHOR_END: sharing

-- 💰 The budget from this place: each instruction once at each place,
--    twice over (once to follow, once to move on). That is the Pike
--    VM's worst only with no marks: with marks just here, a state is an
--    instruction and its marks, and there can be more states than
--    instructions. As a budget it is fine: it grows with the text in
--    proportion. Bytes are counted, not characters: no need to count
--    from the head, and there are never fewer bytes than characters.
-- この場所からの予算：どの命令も、場所ごとに1度ずつ、それを2回分（たどる
-- のと、進めるのと）。Pike VM の最悪になるのは、印がないときだけ。ちょうど
-- ここに印があると、状態は「命令と印」で、命令より多くなりうる。予算として
-- は、これでよい：文字列の長さに比例して増えるだけ。数えるのは文字では
-- なくバイト：頭から数えずに済み、バイトが文字より少ないことはない
budgetFor :: Program -> Cursor -> Int
budgetFor prog c =
  2 * length (instructions prog) * (lengthWord8 (whole c) - byte c + 1)
-- ANCHOR_END: switching

-- ANCHOR: search
-- 🏃 A VM that tries the places in turn: the pure one (scanVM), the
--    one with ST (scanVMST), the Pike VM (scanPike, scanPikeST), the
--    switch between them (scanSwitching), or the one that remembers
--    where it has been (scanBounded, scanShortFirst). All have this
--    type, so the engine can run any of them.
-- 場所を1つずつ試す VM：純粋なもの（scanVM）、ST のもの（scanVMST）、
-- Pike VM（scanPike・scanPikeST）、その乗り換え（scanSwitching）、
-- 通った所を覚えておくもの（scanBounded・scanShortFirst）。どれもこの型
-- なので、エンジンはどれでも動かせる
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
-- 同じもの。ステップ数が尽きたら Left
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
-- しおりから、正規表現が選んだエンジンで。ステップ数が尽きたら Left、
-- searchAt なら GaveUp を投げる
searchWithinAt
  :: Int -> Regex -> Cursor -> Either GaveUp (Maybe (Match, Cursor))
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
