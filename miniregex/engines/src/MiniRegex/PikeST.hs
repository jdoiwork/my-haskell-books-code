-- ANCHOR: all
-- | The Pike VM again, written with ST. It runs the same program, and
--   gives the same answers, as MiniRegex.Pike; only what it keeps is
--   different. The threads at a place are a sparse set of addresses
--   (Russ Cox's way, from Briggs and Torczon): adding one, asking if
--   one is there, and emptying the set all cost the same, however many
--   there are. What each thread's groups found is not a Map: it is a
--   row of numbers ("slots") in one big array, a row for each address.
--   Following a thread through the instructions that take nothing
--   writes the slots of one scratch row in place, and puts the old
--   values back when it turns to the next way (with a stack of things
--   to do, not with recursion).
-- Pike VM を、ST でもう一度。MiniRegex.Pike と同じプログラムを動かし、
-- 同じ答えを出す。ちがうのは、持ち物だけ。ある場所にいるスレッドは、
-- 番地の sparse set（Russ Cox のやり方。もとは Briggs と Torczon）。
-- 足すのも、あるかを聞くのも、空にするのも、いくつあっても同じ手間。
-- スレッドのグループが見つけたものは Map ではなく、1つの大きな配列の中の、
-- 数の行（スロット）。番地ごとに1行。文字を食べない命令をたどるときは、
-- 作業用の1行をその場で書き換え、次の道へ移るときに、前の値を書き戻す
-- （再帰ではなく、やることのスタックで）
module MiniRegex.PikeST
  ( scanPikeST
  , Layout (..)
  , layoutOf
  ) where

-- ANCHOR: imports
import Control.Monad.ST (ST, runST)
import Data.Array.Base (unsafeAt)
import Data.IntMap.Strict qualified as IntMap
import Data.Set (Set)
import Data.Set qualified as Set
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Vector.Unboxed.Mutable qualified as MU

import MiniRegex.Caps (Caps)
import MiniRegex.Cursor hiding (before, rest)
import MiniRegex.Pike (Loops, loopsOf)
import MiniRegex.Program
import MiniRegex.Rules (fitsChar, holds)
import MiniRegex.STVM (slotsOf)
import MiniRegex.Types (Span (..))
import MiniRegex.VM (Answer (..))
-- ANCHOR_END: imports

-- ANCHOR: layout
-- 📐 Where things are in a row of slots. A place in the text is two
--    numbers, the character and the byte; -1 is "not set".
--      0, 1      where the match started
--      2, 3      \K
--      for each group g from 1: where it opened, and where what it
--                found starts and ends (six numbers)
--      then one number for each mark: the character it is at
-- スロットの行の中の並び方。文字列の中の場所は、文字とバイトの2つの数。
-- -1 は「まだ」
--      0, 1      一致が始まった場所
--      2, 3      \K
--      1 番からのグループごとに：開いた場所と、見つけたものの始まりと
--                終わり（6つの数）
--      そのあとに、印ごとに1つ：印のある文字の場所
data Layout = Layout
  { groupsIn :: !Int  -- groups, with 0 / 0 番も入れたグループの数
  , marksIn  :: !Int  -- marks / 印の数
  , width    :: !Int  -- slots in a row / 1行のスロットの数
  }

layoutOf :: Program -> Layout
layoutOf prog = Layout g m (4 + 6 * (g - 1) + m)
  where
    (g, m) = slotsOf prog

openSlot, fromSlot, toSlot :: Int -> Int
openSlot g = 4 + 6 * (g - 1)
fromSlot g = openSlot g + 2
toSlot   g = openSlot g + 4

markSlot :: Layout -> Int -> Int
markSlot lay k = 4 + 6 * (groupsIn lay - 1) + k
-- ANCHOR_END: layout

-- ANCHOR: gen
-- 👥 The threads at one place in the text (one "generation"). `seen`
--    is a sparse set of the addresses already reached here: address i
--    is in it when dense[sparse[i]] == i, below the count. The arrays
--    are never cleared; setting the count to 0 empties the set. `queue`
--    is the addresses that wait to take a character (or to match), best
--    first, and `rows` keeps their slots. A state with marks just here
--    is rare (an empty round of a loop): those go in a plain Set.
-- 文字列のある場所にいるスレッド（1つの「世代」）。seen は、ここで
-- もう着いた番地の sparse set：番地 i は、数より下で
-- dense[sparse[i]] == i なら入っている。配列は消さない。数を 0 に
-- すれば、空になる。queue は、文字を食べる（か一致する）のを待つ番地で、
-- よいものが先。rows に、そのスロットを持つ。ちょうどここに印がある
-- 状態はめったにない（輪の、空の回）ので、ふつうの Set に入れる
data Gen s = Gen
  { sparse  :: !(MU.MVector s Int)
  , dense   :: !(MU.MVector s Int)
  , queue   :: !(MU.MVector s Int)
  , counts  :: !(MU.MVector s Int)  -- [seen, queue] / 2つの数
  , rows    :: !(MU.MVector s Int)
  , marked  :: !(STRef s (Set (Int, [Int])))
  }

newGen :: Int -> Layout -> ST s (Gen s)
newGen size lay =
  Gen <$> MU.replicate size 0
      <*> MU.replicate size 0
      <*> MU.replicate size 0
      <*> MU.replicate 2 0
      <*> MU.replicate (size * width lay) (-1)
      <*> newSTRef Set.empty

-- 🧹 Empty the generation: two counts to 0 (and the rare Set).
-- 世代を空にする：2つの数を 0 に（と、めったにない Set）
{-# INLINE clearGen #-}
clearGen :: Gen s -> ST s ()
clearGen gen = do
  MU.unsafeWrite (counts gen) 0 0
  MU.unsafeWrite (counts gen) 1 0
  writeSTRef (marked gen) Set.empty

-- ❓ Is this the first time address i is reached here? If so, it is
--    in the set from now on.
-- 番地 i に、ここで着いたのは初めてか。初めてなら、いまから set に入る
{-# INLINE firstVisit #-}
firstVisit :: Gen s -> Int -> ST s Bool
firstVisit gen i = do
  n <- MU.unsafeRead (counts gen) 0
  k <- MU.unsafeRead (sparse gen) i
  there <- if k < n
             then (== i) <$> MU.unsafeRead (dense gen) k
             else pure False
  if there
    then pure False
    else do
      MU.unsafeWrite (dense gen) n i
      MU.unsafeWrite (sparse gen) i n
      MU.unsafeWrite (counts gen) 0 (n + 1)
      pure True
-- ANCHOR_END: gen

-- ANCHOR: env
-- 🧰 What every part of a run shares: the program, the layout of the
--    slots, the scratch row, and the stack of things to do. A thing to
--    do is two numbers: (-1, pc) is "go to pc", and (i, v) is "put v
--    back into slot i". Each instruction is reached once at a place,
--    and pushes four things at most, so the stack never grows.
-- 動かすあいだ、どこでも使うもの：プログラム、スロットの並び方、作業用の
-- 行、やることのスタック。やること1つは2つの数：(-1, pc) は「pc へ
-- 行く」、(i, v) は「スロット i に v を書き戻す」。どの命令も、ある場所で
-- 着くのは1度だけで、積むのは多くても4つなので、スタックは伸びない
data Env s = Env
  { prog     :: !Program
  , lay      :: !Layout
  , hasMarks :: !Bool
  , loops    :: !Loops  -- the repeat of each mark / 印ごとのくり返し
  , accept   :: Cursor -> Bool
  , scratch  :: !(MU.MVector s Int)
  , todo     :: !(MU.MVector s Int)
  }
-- ANCHOR_END: env

-- ANCHOR: scan
-- 🔁 The same type as scanPike, and the same steps (but see `follow`:
--    two threads waiting at the same take are one here).
-- scanPike と同じ型で同じステップ数（ただし follow を見てほしい。同じ take で
-- 待つ2本のスレッドは、ここでは1本）
scanPikeST
  :: Program -> Int -> (Cursor -> Bool) -> [Cursor]
  -> (Maybe Cursor, Answer, Int)
scanPikeST _ limit _ [] = (Nothing, NotFound, limit)
scanPikeST program limit ok places@(first : _) = runST $ do
  let layout = layoutOf program
      size   = length (instructions program)
  now   <- newGen size layout
  later <- newGen size layout
  env <- Env program layout (any isMark (instructions program))
             (loopsOf program) ok
           <$> MU.replicate (width layout) (-1)
           <*> MU.replicate (2 * (4 * size + 1)) 0
  run env first places now later limit Nothing
  where
    isMark i = case i of
      SetMark _ -> True
      _         -> False
-- ANCHOR_END: scan

-- ANCHOR: run
-- 🚶 At place c: start a thread here (if a match may still start
--    here), move everyone on by the character, and go on.
-- 場所 c で：ここで始まるスレッドを足し（まだ一致が始まりうるなら）、
-- 全員を文字で進め、先へ行く
type Found = (Cursor, Cursor, Caps)

run
  :: Env s -> Cursor -> [Cursor] -> Gen s -> Gen s -> Int -> Maybe Found
  -> ST s (Maybe Cursor, Answer, Int)
run env !c starts now later !left best = do
  -- a thread for a match starting here, after all the others
  -- ここで始まる一致のスレッドを、ほかの全員のあとに
  (left1, starts') <- case (best, starts) of
    (Nothing, p : ps) | at p == at c -> do
      MU.set (scratch env) (-1)
      MU.unsafeWrite (scratch env) 0 (at c)
      MU.unsafeWrite (scratch env) 1 (byte c)
      l <- follow env now c 0 left
      pure (l, ps)
    _ -> pure (left, starts)
  clearGen later
  (left2, best') <- advance env c now later left1 best
  waiting <- MU.unsafeRead (counts later) 1
  if left2 < 0
    then pure (Nothing, OutOfSteps, 0)
    else case (waiting, best', starts', step c) of
      (0, Just _, _, _)       -> pure (done best' left2)
      (0, _, [], _)           -> pure (done best' left2)
      -- nobody is left: go straight to the next place to start
      -- だれも残らない：次に始める場所へ、まっすぐ行く
      (0, _, p : _, _)        -> do
        clearGen now
        run env p starts' now later left2 best'
      -- the next generation is now; this one, emptied, will be next
      -- 次の世代が、いまに。いまの世代は、空にして次に使う
      (_, _, _, Just (_, c')) -> run env c' starts' later now left2 best'
      (_, _, _, Nothing)      -> pure (done best' left2)
  where
    done b l = case b of
      Just (o, end, cs) -> (Just o, Found end cs, l)
      Nothing           -> (Nothing, NotFound, l)
-- ANCHOR_END: run

-- ANCHOR: advance
-- 🏃 Move the waiting threads on by the character at c, best first,
--    into the next generation. A match is kept; the threads after it
--    are worse, and dropped.
-- 待っているスレッドを、c の文字で、よいものから順に、次の世代へ進める。
-- 一致は覚えて、そのあとのスレッドは、もっと悪いので捨てる
advance
  :: Env s -> Cursor -> Gen s -> Gen s -> Int -> Maybe Found
  -> ST s (Int, Maybe Found)
advance env c now later left0 best0 = do
  n <- MU.unsafeRead (counts now) 1
  let go !i !left best
        | i >= n || left < 0 = pure (left, best)
        | otherwise = do
            pc <- MU.unsafeRead (queue now) i
            let counted = spend left
            case unsafeAt (prog env) pc of
              Match
                | accept env c -> do
                    found <- frozen env (rows now) pc c
                    pure (counted, Just found)
                | otherwise -> go (i + 1) counted best
              Take a    -> eat a pc (pc + 1) counted >>= \l -> go (i + 1) l best
              -- one more: the thread stays at the takeall
              -- もう1文字：スレッドは takeall にとどまる
              TakeAll a -> eat a pc pc counted >>= \l -> go (i + 1) l best
              _         -> go (i + 1) counted best  -- (not reached / 来ない)
  go 0 left0 best0
  where
    next = step c
    w    = width (lay env)
    eat a pc to left = case next of
      Just (x, c') | fitsChar a x -> do
        MU.unsafeCopy (scratch env) (MU.unsafeSlice (pc * w) w (rows now))
        follow env later c' to left
      _ -> pure left
-- ANCHOR_END: advance

-- ANCHOR: follow
-- 🌿 Follow a thread from address pc0 at place c, best way first,
--    through the instructions that take no character. Its slots are in
--    the scratch row. Where it waits (a take, a takeall, a match), the
--    slots are copied into the generation's row for that address. Each
--    instruction followed is one step, as in scanPike.
-- 場所 c で、番地 pc0 からスレッドをたどる。よい道から順に、文字を
-- 食べない命令を。スロットは作業用の行にある。待つ所（take・takeall・
-- match）で、その番地の世代の行に、スロットを写す。たどった命令1つが
-- 1ステップ（scanPike と同じ）
follow :: Env s -> Gen s -> Cursor -> Int -> Int -> ST s Int
follow env gen c pc0 left0 = explore 0 pc0 left0
  where
    sc  = scratch env
    lo  = lay env
    -- the next thing to do, from the top of the stack
    -- スタックのいちばん上から、次にやること
    loop !sp !left
      | sp == 0   = pure left
      | otherwise = do
          i <- MU.unsafeRead (todo env) (2 * sp - 2)
          v <- MU.unsafeRead (todo env) (2 * sp - 1)
          if i < 0
            then explore (sp - 1) v left
            else MU.unsafeWrite sc i v >> loop (sp - 1) left

    explore !sp !pc !left
      | left < 0  = pure left
      | otherwise = do
          new <- fresh pc
          if not new then loop sp left else case unsafeAt (prog env) pc of
            Take _    -> wait pc >> loop sp left
            Match     -> wait pc >> loop sp left
            -- wait to take one more, and go on without (worse)
            -- もう1文字を待ち、食べずに先へも進む（悪いほう）
            TakeAll _ -> wait pc >> explore sp (pc + 1) (spend left)
            Assert a
              | holds a c -> explore sp (pc + 1) (spend left)
              | otherwise -> loop sp (spend left)
            -- the second way waits on the stack, under what the first
            -- way will put back
            -- 2つ目の道は、1つ目の道が書き戻すものの下で待つ
            Split a b -> do
              sp' <- push sp (-1) b
              explore sp' a (spend left)
            Jmp a     -> explore sp a (spend left)
            Open g    -> do
              sp' <- setPlace (openSlot g) sp
              explore sp' (pc + 1) (spend left)
            Close g   -> do
              o  <- MU.unsafeRead sc (openSlot g)
              ob <- MU.unsafeRead sc (openSlot g + 1)
              if o < 0
                then loop sp (spend left)  -- (not reached / 来ない)
                else do
                  sp1 <- set (fromSlot g) o sp
                  sp2 <- set (fromSlot g + 1) ob sp1
                  sp3 <- setPlace (toSlot g) sp2
                  explore sp3 (pc + 1) (spend left)
            Keep      -> do
              sp' <- setPlace 2 sp
              explore sp' (pc + 1) (spend left)
            SetMark k -> do
              sp' <- set (markSlot lo k) (at c) sp
              explore sp' (pc + 1) (spend left)
            IfStill k a -> do
              m <- MU.unsafeRead sc (markSlot lo k)
              explore sp (if m == at c then a else pc + 1) (spend left)
            other -> error ("the Pike VM cannot run " ++ show (() <$ other))

    -- Is this state new here? A take or a match is one state by its
    -- address alone: marks just here change nothing once a character
    -- is taken. Other instructions with marks just here go to the Set.
    -- この状態は、ここで新しいか。take と match は、番地だけで1つの状態：
    -- ちょうどここにある印は、1文字食べたあとは何も変えない。ほかの命令で、
    -- ちょうどここに印があるものは、Set へ
    fresh pc
      | hasMarks env && not (waits (unsafeAt (prog env) pc)) = do
          here <- marksHere pc
          if null here then firstVisit gen pc else do
            s <- readSTRef (marked gen)
            if Set.member (pc, here) s
              then pure False
              else do
                modifySTRef' (marked gen) (Set.insert (pc, here))
                pure True
      | otherwise = firstVisit gen pc
    -- only the marks of the repeats this thread is inside, as in the
    -- Pike VM's stateAt (with every mark, k repeats in a row that can
    -- take nothing would make 2^k states)
    -- このスレッドがいま中にいるくり返しの印だけ。Pike VM の stateAt と同じ
    -- （全部の印だと、何も食べずに済むくり返しを k 個並べただけで 2 の k 乗）
    marksHere pc = do
      ms <- mapM (MU.unsafeRead sc . markSlot lo) [0 .. marksIn lo - 1]
      pure [k | (k, p) <- zip [0 ..] ms, p == at c, inside k]
      where
        inside k = case IntMap.lookup k (loops env) of
          Just (from, to) -> from < pc && pc <= to
          Nothing         -> True
    waits i = case i of
      Take _    -> True
      TakeAll _ -> True
      Match     -> True
      _         -> False

    -- wait here: into the queue, with a copy of the slots
    -- ここで待つ：queue に入れ、スロットを写す
    wait pc = do
      n <- MU.unsafeRead (counts gen) 1
      MU.unsafeWrite (queue gen) n pc
      MU.unsafeWrite (counts gen) 1 (n + 1)
      let w = width lo
      MU.unsafeCopy (MU.unsafeSlice (pc * w) w (rows gen)) sc

    -- write a slot, and push a note to put the old value back
    -- スロットを書き、前の値を書き戻すメモを積む
    set i v sp = do
      old <- MU.unsafeRead sc i
      MU.unsafeWrite sc i v
      push sp i old
    setPlace i sp = set i (at c) sp >>= set (i + 1) (byte c)
    push sp i v = do
      MU.unsafeWrite (todo env) (2 * sp) i
      MU.unsafeWrite (todo env) (2 * sp + 1) v
      pure (sp + 1)
-- ANCHOR_END: follow

-- ANCHOR: frozen
-- 🧊 A match: where it started, where it ended, and the groups, as the
--    table the rest of MiniRegex knows.
-- 一致：始まった場所、終わった場所、グループを、MiniRegex のほかの
-- 部分が知っている表にして
frozen :: Env s -> MU.MVector s Int -> Int -> Cursor -> ST s Found
frozen env rs pc c = do
  row <- mapM (\i -> MU.read rs (pc * w + i)) [0 .. w - 1]
  let slot i = row !! i
      place a b = jumpTo a b (headOf (whole c))
      spanOf g =
        let from = place (slot (fromSlot g)) (slot (fromSlot g + 1))
            to   = place (slot (toSlot g)) (slot (toSlot g + 1))
        in Span (taken from to) (at from) (at to)
      keep  = [(0, Span "" (slot 2) (slot 2)) | slot 2 >= 0]
      found = [(g, spanOf g) | g <- [1 .. groupsIn (lay env) - 1]
                             , slot (toSlot g) >= 0]
      caps :: Caps
      caps = IntMap.fromList (keep ++ found)
  pure (place (slot 0) (slot 1), c, caps)
  where
    w = width (lay env)
-- ANCHOR_END: frozen

-- 👣 One step. With none left, -1: they ran out.
-- 1ステップ使う。もう残っていなければ -1：尽きた
{-# INLINE spend #-}
spend :: Int -> Int
spend left
  | left <= 0 = -1
  | otherwise = left - 1
-- ANCHOR_END: all
