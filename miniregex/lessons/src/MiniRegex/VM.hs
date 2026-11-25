-- ANCHOR: all
-- | The backtracking VM. It runs a program one instruction at a time,
--   and keeps the other ways to try on a stack of its own, as data.
--   Each instruction it runs is one step; when the steps run out, it
--   gives up instead of going on for ever.
-- バックトラックの VM。プログラムを1命令ずつ動かし、ほかの試し方を、
-- 自分で持つスタックに、データとして積む。命令を1つ動かすのが1ステップ。
-- ステップ数が尽きたら、いつまでも続けずに、あきらめる
module MiniRegex.VM
  ( Thread (..)
  , Frame (..)
  , Entry (..)
  , Answer (..)
  , runVM
  , scanVM
  ) where

-- ANCHOR: imports
import Data.Array ((!))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap

import MiniRegex.Caps
import MiniRegex.Cursor hiding (before, rest)
import MiniRegex.Program
import MiniRegex.Rules (eatAll, fitsChar, holds)
import MiniRegex.Types (Span (..))
import MiniRegex.Unicode (sameCase)
-- ANCHOR_END: imports

-- ANCHOR: thread
-- 🧵 Where the VM is: the next instruction, the place in the text, what
--    the groups found, where the open groups started, the marks, and
--    the calls it is in (the latest first).
-- VM がいまどこにいるか：次の命令、文字列の中の場所、グループが
-- 見つけたもの、開いているグループの始まり、印、いま中にいる呼び出し
-- （新しいものが先頭）
data Thread = Thread
  { pc     :: !Int
  , cursor :: !Cursor
  , caps   :: !Caps
  , opens  :: !(IntMap Cursor)
  , marks  :: !(IntMap Int)
  , calls  :: ![Frame]
  }

-- 📞 One call: where to come back, which group was called and where in
--    the text, and the groups, open groups and marks as they were.
-- 呼び出し1つ：戻ってくる番地、どのグループを文字列のどこで呼んだか、
-- そのときのグループ・開いたグループ・印
data Frame = Frame
  { returnTo  :: !Int
  , calledAs  :: !Int
  , calledAt  :: !Int
  , oldCaps   :: !Caps
  , oldOpens  :: !(IntMap Cursor)
  , oldMarks  :: !(IntMap Int)
  }

-- 🧱 What the stack holds: another way to try, or a fence. A fence
--    keeps the thread from when it was put; a fence of a negative look
--    also keeps where to go on when everything inside fails.
-- スタックに積むもの：ほかの試し方か、区切り。区切りは、積んだときの
-- スレッドを持つ。否定の「見る」ものの区切りは、中が全部だめだったときに
-- 進む先も持つ
data Entry
  = Retry Thread
  | Wall (Maybe Int) Thread

-- 🏁 How it ended.
-- どう終わったか
data Answer
  = Found Cursor Caps
  | NotFound
  | OutOfSteps
  -- group g was called again at place p, inside a call of itself at p
  -- p の場所で g を呼んだ中で、また同じ p で g を呼んだ
  | Looped Int Int
-- ANCHOR_END: thread

-- ANCHOR: run
-- 🏃 Run the program from the cursor, with this many steps at most.
--    `accept` says whether a Match at a place counts (for findAll, a
--    place where the match is empty may not). Gives back the answer and
--    the steps that are left.
-- しおりから、多くてもこのステップ数で、プログラムを動かす。accept は、その
-- 場所の Match を認めるか（findAll では、空の一致を認めないことがある）。
-- 答えと、残りのステップ数を返す
runVM :: Program -> Int -> (Cursor -> Bool) -> Cursor -> (Answer, Int)
runVM prog limit accept start =
  go limit (Thread 0 start noCaps IntMap.empty IntMap.empty []) []
  where
    go :: Int -> Thread -> [Entry] -> (Answer, Int)
    go n t stack
      | n <= 0    = (OutOfSteps, 0)
      | otherwise = exec (n - 1) (prog ! pc t) t stack
-- ANCHOR_END: run

    -- ANCHOR: exec
    -- ⚙️ Run one instruction; n steps are left after it.
    -- 命令を1つ動かす。そのあとの残りのステップ数は n
    exec n inst t stack = case inst of
      Take a -> case step (cursor t) of
        Just (x, c) | fitsChar a x -> next t { cursor = c }
        _                          -> failed n stack
      Assert a
        | holds a (cursor t) -> next t
        | otherwise          -> failed n stack
      -- Go on to the first; put the second on the stack, for later.
      -- 1つ目へ進む。2つ目は、あとのためにスタックに積む
      Split a b -> go n t { pc = a } (Retry t { pc = b } : stack)
      Jmp a     -> go n t { pc = a } stack
      Open g    -> next t { opens = IntMap.insert g (cursor t) (opens t) }
      Close g   -> case IntMap.lookup g (opens t) of
        Just from -> next t { caps = remember g from (cursor t) (caps t) }
        Nothing   -> failed n stack  -- (not reached / 来ない)
      Ref g       -> again stepOver g
      RefNoCase g -> again (stepOverBy sameCase) g
      Keep  -> next t { caps = keepOut (cursor t) (caps t) }
      Match
        | accept (cursor t) -> (Found (cursor t) (caps t), n)
        | otherwise         -> failed n stack
      -- ANCHOR_END: exec
      -- ANCHOR: execmarks
      SetMark m -> next t { marks = IntMap.insert m here (marks t) }
      IfStill m a
        | still m   -> go n t { pc = a } stack
        | otherwise -> next t
      -- ANCHOR_END: execmarks
      -- ANCHOR: execfences
      Fence      -> next' (Wall Nothing t : stack)
      FenceNot a -> next' (Wall (Just a) t : stack)
      Cut     -> withFence (\_ below -> go n t { pc = pc t + 1 } below)
      CutBack -> withFence (\w below ->
        go n t { pc = pc t + 1, cursor = cursor w } below)
      CutFail -> withFence (\_ below -> failed n below)
      -- ANCHOR_END: execfences
      -- ANCHOR: execbehind
      Back d -> case back d (cursor t) of
        Just c  -> next t { cursor = c }
        Nothing -> failed n stack
      AtMark m
        | still m   -> next t
        | otherwise -> failed n stack
      -- ANCHOR_END: execbehind
      -- ANCHOR: execcall
      Gosub g a
        | looping g (calls t) -> (Looped g here, n)
        | otherwise -> go n t { pc = a, calls = frame : calls t } stack
        where
          frame = Frame (pc t + 1) g here (caps t) (opens t) (marks t)
      Return -> case calls t of
        f : outer -> go n t { pc       = returnTo f
                            , caps     = returning (oldCaps f) (caps t)
                            , opens    = oldOpens f
                            , marks    = oldMarks f
                            , calls    = outer } stack
        []        -> failed n stack  -- (not reached / 来ない)
      -- ANCHOR_END: execcall
      -- ANCHOR: exectakeall
      TakeAll a -> case eatAll a n (cursor t) of
        (c, n') | n' > 0    -> go n' t { pc = pc t + 1, cursor = c } stack
                | otherwise -> (OutOfSteps, 0)
      -- ANCHOR_END: exectakeall
      -- ANCHOR: where
      where
        next t'  = go n t' { pc = pc t + 1 } stack
        next' s  = go n t { pc = pc t + 1 } s
        still m  = IntMap.lookup m (marks t) == Just here
        here     = at (cursor t)
        again over g = case recall g (caps t) >>= stepped over of
          Just c  -> next t { cursor = c }
          Nothing -> failed n stack
        stepped over s = over (spanText s) (cursor t)
        withFence k = case cut stack of
          Just (w, below) -> k w below
          Nothing         -> failed n stack  -- (not reached / 来ない)
        -- a call of the same group, at the same place, with the same
        -- groups: everything from here would be the same again, for
        -- ever. (Only the place is not enough: a look can make the
        -- groups grow, and then the calls end.)
        -- 同じグループの呼び出しが、同じ場所で、同じグループの値で、前にも
        -- あった：ここから先は、いつまでも同じになる（場所だけでは足りない。
        -- 「見る」ものがグループを伸ばしていれば、呼び出しは終わる）
        -- (the places only go down, deeper into the list: only the calls
        -- at this same place, at its head, need looking at)
        -- （場所は、リストの奥へ向かって小さくなるだけ：見るのは、頭の、
        -- 同じ場所の呼び出しだけ）
        looping g fs =
          any (\f -> calledAs f == g && oldCaps f == caps t)
              (takeWhile ((== here) . calledAt) fs)
      -- ANCHOR_END: where

    -- ANCHOR: failed
    -- ↩️ This way failed: take the next way to try from the stack. A
    --    fence on the way is just thrown away, unless it is a negative
    --    look's: then everything inside has failed, and the look holds.
    -- この道は失敗。スタックから、次の試し方を取る。途中の区切りは捨てる
    -- だけ。ただし否定の「見る」ものの区切りなら、中が全部だめだったので、
    -- 「見る」ものは成り立つ
    failed n []                       = (NotFound, n)
    failed n (Retry t : rest)         = go n t rest
    failed n (Wall Nothing _ : rest)  = failed n rest
    failed n (Wall (Just a) t : rest) = go n t { pc = a } rest
    -- ANCHOR_END: failed

-- ANCHOR: scan
-- 🔁 Run the program from each place in turn, sharing the steps, until
--    one finds a match or the steps run out. Gives back the place it
--    stopped at (Nothing if every place failed), the answer, and the
--    steps left.
-- 場所を1つずつ、ステップ数を分け合って、一致が見つかるか尽きるまで
-- 動かす。止まった場所（失敗なら Nothing）、答え、残りのステップ数を返す
scanVM
  :: Program -> Int -> (Cursor -> Bool) -> [Cursor]
  -> (Maybe Cursor, Answer, Int)
scanVM _ n _ [] = (Nothing, NotFound, n)
scanVM prog n accept (c : cs) = case runVM prog n accept c of
  (NotFound, left) -> scanVM prog left accept cs
  (answer, left)   -> (Just c, answer, left)
-- ANCHOR_END: scan

-- ANCHOR: cut
-- ✂️ Throw away the ways to try down to the nearest fence; give back
--    the fence's thread, and what is below it.
-- いちばん近い区切りまでの試し方を捨てる。区切りのスレッドと、その下を
-- 返す
cut :: [Entry] -> Maybe (Thread, [Entry])
cut (Retry _ : rest)  = cut rest
cut (Wall _ w : rest) = Just (w, rest)
cut []                = Nothing
-- ANCHOR_END: cut
-- ANCHOR_END: all
