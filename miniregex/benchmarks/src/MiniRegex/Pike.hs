-- ANCHOR: all
-- | The Pike VM. It runs the same program as the backtracking VM, but
--   it never goes back: it keeps every place in the program it could be
--   at, as threads, and moves them all on by one character together.
--   Two threads at the same state are one, so the threads are never
--   more than the instructions, and the time grows in proportion to
--   the length of the text. Programs with back references, calls,
--   fences or lookbehinds cannot run on it.
-- Pike VM。バックトラックの VM と同じプログラムを動かすが、決して戻らない。
-- プログラムの中で、いま居られる場所を全部スレッドとして持ち、全員を
-- いっしょに1文字ずつ進める。同じ状態のスレッドは1つにまとめるので、
-- スレッドの数は命令の数を超えず、時間は文字列の長さに比例して伸びる。
-- 後方参照・呼び出し・区切り・後読みのあるプログラムは、動かせない
module MiniRegex.Pike
  ( Thread (..)
  , Crowd (..)
  , scanPike
  , blocker
  , runnable
  ) where

-- ANCHOR: imports
import Data.Array ((!))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (find)
import Data.Set (Set)
import Data.Set qualified as Set

import MiniRegex.Caps
import MiniRegex.Cursor hiding (before, rest)
import MiniRegex.Program
import MiniRegex.Rules (fitsChar, holds)
import MiniRegex.VM (Answer (..))
-- ANCHOR_END: imports

-- ANCHOR: runnable
-- ✅ Can the Pike VM run this instruction? Not the ones that look
--    back at what a group found (ref), call (gosub, return), or throw
--    away the other ways to try (fence, cut ...), or step back.
-- この命令を Pike VM で動かせるか。グループが見つけたものを見返すもの
-- （ref）、呼ぶもの（gosub・return）、ほかの試し方を捨てるもの
-- （fence・cut など）、下がるものは、動かせない
runnable :: Inst l -> Bool
runnable inst = case inst of
  Take _      -> True
  Assert _    -> True
  Split _ _   -> True
  Jmp _       -> True
  Open _      -> True
  Close _     -> True
  Keep        -> True
  Match       -> True
  SetMark _   -> True
  IfStill _ _ -> True
  TakeAll _   -> True
  _           -> False

-- 🚧 The first instruction of the program that the Pike VM cannot
--    run (Nothing: it can run them all).
-- プログラムの中で、Pike VM が動かせない最初の命令（Nothing なら、
-- 全部動かせる）
blocker :: Program -> Maybe (Inst Int)
blocker prog = find (not . runnable) (instructions prog)
-- ANCHOR_END: runnable

-- ANCHOR: thread
-- 🧵 One thread: the next instruction, where its match started, what
--    the groups found, where the open groups started, and the marks.
--    The place in the text is not here: every thread is at the same
--    place.
-- スレッド1本：次の命令、一致が始まった場所、グループが見つけたもの、
-- 開いているグループの始まり、印。文字列の中の場所は持たない。どの
-- スレッドも、同じ場所にいるから
data Thread = Thread
  { pc     :: !Int
  , origin :: !Cursor
  , caps   :: !Caps
  , opens  :: !(IntMap Cursor)
  , marks  :: !(IntMap Int)
  }

-- 👥 The threads waiting at one place, the best first (kept in
--    reverse while they are added), the states already taken there,
--    and the steps left (-1: they ran out).
-- ある場所で待っているスレッド（よいものが先。足すあいだは逆順に
-- 持つ）、そこで、もう使われた状態、残りの手数（-1 なら尽きた）
data Crowd = Crowd
  { waiting :: ![Thread]
  , visited :: !(Set (Int, [Mark]))
  , left    :: !Int
  }
-- ANCHOR_END: thread

-- ANCHOR: state
-- 🔑 The state of a thread at a place: the instruction, and which
--    marks are just here. Nothing else can change what it does next
--    (the groups are only written, never read: there is no ref).
-- ある場所でのスレッドの状態：命令と、どの印がちょうどここにあるか。
-- これからの動きを変えるものは、ほかにない（グループは書くだけで、
-- 読まれない。ref がないから）
stateAt :: Cursor -> Thread -> (Int, [Mark])
stateAt c t = (pc t, [m | (m, p) <- IntMap.toList (marks t), p == at c])
-- ANCHOR_END: state

-- ANCHOR: follow
-- 🌿 Add a thread at place c, and follow it through the instructions
--    that take no character, best way first, until it waits at a take
--    or a match. A state already taken at c is not taken again: the
--    thread there came first, so it is better. Each instruction
--    followed is one step.
-- 場所 c にスレッドを足し、文字を食べない命令を、よい道から順にたどる。
-- take か match で待つまで。c でもう使われた状態には、もう入らない。
-- 先に来たスレッドのほうがよいから。たどった命令1つが1手
follow :: Program -> Cursor -> Thread -> Crowd -> Crowd
follow prog c t crowd
  | left crowd < 0                  = crowd
  | Set.member state (visited crowd)    = crowd
  | otherwise = case prog ! pc t of
      Take _ -> wait
      Match  -> wait
      Assert a
        | holds a c -> next t marked
        | otherwise -> spent
      -- the first way, then the second: the order is the priority
      -- 1つ目の道、それから2つ目。この順番が、優先の順番
      Split a b -> follow prog c t { pc = b } (follow prog c t { pc = a } spent)
      Jmp a     -> follow prog c t { pc = a } spent
      Open g    -> next t { opens = IntMap.insert g c (opens t) } marked
      Close g   -> case IntMap.lookup g (opens t) of
        Just from -> next t { caps = remember g from c (caps t) } marked
        Nothing   -> spent  -- (not reached / 来ない)
      Keep      -> next t { caps = keepOut c (caps t) } marked
      SetMark m -> next t { marks = IntMap.insert m (at c) (marks t) } marked
      IfStill m a
        | IntMap.lookup m (marks t) == Just (at c) ->
            follow prog c t { pc = a } spent
        | otherwise -> next t marked
      -- Wait here to take one more, and (worse) go on without.
      -- ここで、もう1文字食べるのを待つ。それより悪い道として、
      -- 食べずに先へも進む
      TakeAll _ -> next t (wait)
      other -> error ("the Pike VM cannot run " ++ show (() <$ other))
  where
    state  = stateAt c t
    marked = crowd { visited = Set.insert state (visited crowd) }
    -- wait here for the next character (the step is counted then)
    -- ここで次の文字を待つ（手数は、そのときに数える）
    wait   = marked { waiting = t : waiting marked }
    spent  = spend marked
    next t' cr = follow prog c t' { pc = pc t + 1 } (spend cr)

-- 👣 One step. With none left, the crowd has run out.
-- 1手使う。もう残っていなければ、尽きた印を付ける
spend :: Crowd -> Crowd
spend crowd
  | left crowd <= 0 = crowd { left = -1 }
  | otherwise       = crowd { left = left crowd - 1 }
-- ANCHOR_END: follow

-- ANCHOR: scan
-- 🔁 Search from the places in turn (a list of places one after
--    another, or a single one), all at once. A new thread for a match
--    that starts at a place is added after the others: it is the
--    worst. Gives back where the match started, the answer, and the
--    steps left: the same as the backtracking VM's scanVM.
-- 場所を順に（続いた場所のリストか、場所1つ）、いっぺんに探す。ある場所で
-- 始まる一致のための新しいスレッドは、ほかのスレッドのあとに足す。いちばん
-- 悪いから。一致が始まった場所、答え、残りの手数を返す。バックトラックの
-- VM の scanVM と同じ
scanPike
  :: Program -> Int -> (Cursor -> Bool) -> [Cursor]
  -> (Maybe Cursor, Answer, Int)
scanPike _ limit _ [] = (Nothing, NotFound, limit)
scanPike prog limit accept places@(first : _) =
  loop first places (Crowd [] Set.empty limit) Nothing
  where
    loop c todo crowd best
      | left after < 0 = (Nothing, OutOfSteps, 0)
      | otherwise = case (waiting after, best', todo', step c) of
          -- nobody is left, and no new match can start: done
          -- だれも残らず、新しい一致も始まらない：おしまい
          ([], Just _, _, _)   -> done
          ([], _, [], _)       -> done
          -- nobody is left: go straight to the next place to start
          -- だれも残らない：次に始める場所へ、まっすぐ行く
          ([], _, p : _, _)    -> loop p todo' (fresh (left after)) best'
          (_, _, _, Just (_, c')) -> loop c' todo' after best'
          (_, _, _, Nothing)   -> done
      where
        -- a thread for a match starting here, last of all
        -- ここで始まる一致のスレッドを、いちばん最後に
        (crowd', todo') = case (best, todo) of
          (Nothing, p : ps) | at p == at c ->
            (follow prog c (Thread 0 c noCaps IntMap.empty IntMap.empty) crowd, ps)
          _ -> (crowd, todo)
        -- move every thread on by the character here
        -- 全員を、ここの文字で進める
        (after, best') =
          advance c (reverse (waiting crowd')) (fresh (left crowd')) best
        done = case best' of
          Just (o, end, cs) -> (Just o, Found end cs, left after)
          Nothing           -> (Nothing, NotFound, left after)

    fresh = Crowd [] Set.empty

    -- 🏃 Run the waiting threads at c, best first. A take that fits
    --    goes on to the next place; a match is kept, and the threads
    --    after it are dropped (they are worse). A match found later,
    --    by a better thread, takes its place.
    -- c で待っているスレッドを、よいものから動かす。合う take は、次の
    -- 場所へ進む。match は覚えて、そのあとのスレッドは捨てる（もっと
    -- 悪いから）。あとで、もっとよいスレッドが見つけた一致は、それと
    -- 入れ替わる
    advance _ [] nextCrowd best = (nextCrowd, best)
    advance c (t : ts) nextCrowd best
      | left nextCrowd < 0 = (nextCrowd, best)
      | otherwise = case prog ! pc t of
          Match
            | accept c  -> (counted, Just (origin t, c, caps t))
            | otherwise -> advance c ts counted best
          Take a -> case step c of
            Just (x, c') | fitsChar a x ->
              advance c ts (follow prog c' t { pc = pc t + 1 } counted) best
            _ -> advance c ts counted best
          -- one more: the thread stays at the takeall
          -- もう1文字：スレッドは takeall にとどまる
          TakeAll a -> case step c of
            Just (x, c') | fitsChar a x ->
              advance c ts (follow prog c' t counted) best
            _ -> advance c ts counted best
          _ -> advance c ts counted best  -- (not reached / 来ない)
      where
        counted = spend nextCrowd
-- ANCHOR_END: scan
-- ANCHOR_END: all
