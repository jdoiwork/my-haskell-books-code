-- ANCHOR: all
-- | The backtracking VM again, written with ST. It runs the same
--   program, one instruction a step, the same as MiniRegex.VM; only
--   what it keeps is different. The groups and the marks are mutable
--   arrays, one of each, written over in place. Each time it writes
--   over one, it puts the old value on the trail; going back to a way
--   to try, it puts back the old values, down to where the trail was.
-- バックトラックの VM を、ST でもう一度。MiniRegex.VM と同じ
-- プログラムを、同じく1命令1手で動かす。ちがうのは、持ち物だけ。
-- グループと印は、書き換えられる配列を1つずつ持ち、その場で書き換える。
-- 書き換えるたびに、前の値を「取り消しの記録」に積む。ほかの試し方へ
-- 戻るときは、そのときの記録の高さまで、前の値を書き戻す
module MiniRegex.STVM
  ( scanVMST
  , scanVMSTFresh
  , Choice (..)
  , Undo (..)
  , slotsOf
  ) where

-- ANCHOR: imports
import Control.Monad.ST (ST, runST)
import Data.Array ((!))
import Data.IntMap.Strict qualified as IntMap
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed.Mutable qualified as MU

import MiniRegex.Caps (Caps, keptAt, spanBetween)
import MiniRegex.Cursor
import MiniRegex.Program
import MiniRegex.Rules (fitsChar, holds)
import MiniRegex.Stack
import MiniRegex.Types (Span (..))
import MiniRegex.Unicode (sameCase)
import MiniRegex.VM (Answer (..))
-- ANCHOR_END: imports

-- ANCHOR: choice
-- 🔀 A way to try later: where to go on, where in the text, and how
--    high the trail was. A fence keeps the place and the trail's
--    height, too; a negative look's fence also keeps where to go when
--    everything inside fails.
-- あとで試す道：続ける番地、文字列の中の場所、そのときの記録の高さ。
-- 区切りも、場所と記録の高さを持つ。否定の「見る」ものの区切りは、
-- 中が全部だめだったときに進む先も持つ
data Choice
  = Retry !Int !Cursor !Int
  | Wall !(Maybe Int) !Cursor !Int

-- 📝 What was there before a write: a group's span, the start of an
--    open group, or a mark.
-- 書き換える前に、そこにあったもの：グループの場所、開いたグループの
-- 始まり、印
data Undo
  = UndoCap  !Int !(Maybe Span)
  | UndoOpen !Int !(Maybe Cursor)
  | UndoMark !Int !Int
-- ANCHOR_END: choice

-- ANCHOR: machine
-- 🏭 The machine's things, made once at the start of a run: the
--    groups (drawer 0 is for \K), the starts of the open groups, the
--    marks (-1 for "not set"), and the two stacks.
-- 機械の持ち物。動かし始めるときに1度だけ作る：グループ（0番の
-- 引き出しは \K）、開いたグループの始まり、印（-1 は「まだ」）、
-- 2つのスタック
data Machine s = Machine
  { capsOf   :: !(MV.MVector s (Maybe Span))
  , opensOf  :: !(MV.MVector s (Maybe Cursor))
  , marksOf  :: !(MU.MVector s Int)
  , choices  :: !(Stack s Choice)
  , trail    :: !(Stack s Undo)
  }

-- 📐 How many drawers the program needs: the biggest group number,
--    and the biggest mark number, each plus one.
-- プログラムに要る引き出しの数：いちばん大きいグループの番号と、
-- いちばん大きい印の番号に、それぞれ1を足したもの
slotsOf :: Program -> (Int, Int)
slotsOf prog = (1 + maximum (0 : groups), 1 + maximum (-1 : marks))
  where
    is     = instructions prog
    groups = [g | i <- is, Just g <- [groupOf i]]
    marks  = [m | i <- is, Just m <- [markOf i]]
    groupOf i = case i of
      Open g      -> Just g
      Close g     -> Just g
      Ref g       -> Just g
      RefNoCase g -> Just g
      _           -> Nothing
    markOf i = case i of
      SetMark m   -> Just m
      IfStill m _ -> Just m
      AtMark m    -> Just m
      _           -> Nothing

-- 🆕 Make the machine's things.
-- 機械の持ち物を作る
newMachine :: (Int, Int) -> ST s (Machine s)
newMachine (groups, marks) =
  Machine <$> MV.replicate groups Nothing
          <*> MV.replicate groups Nothing
          <*> MU.replicate marks (-1)
          <*> newStack
          <*> newStack
-- ANCHOR_END: machine

-- ANCHOR: run
-- 🏃 The same type as scanVM: run the program from each place in turn,
--    sharing the steps. The machine's things are made once, for all
--    the places; between two places, the trail puts everything back.
--    Outside, nobody can tell it writes over anything: runST shuts the
--    writing in.
-- scanVM と同じ型：場所を1つずつ、手数を分け合いながら動かす。機械の
-- 持ち物は、どの場所にも使えるように1度だけ作る。場所と場所のあいだは、
-- 取り消しの記録が全部を元に戻す。書き換えていることは、外からは
-- わからない。runST が閉じ込める
scanVMST
  :: Program -> Int -> (Cursor -> Bool) -> [Cursor]
  -> (Maybe Cursor, Answer, Int)
scanVMST prog limit accept places = runST $ do
  m <- newMachine (slotsOf prog)
  scan m limit places
  where
    scan :: forall s. Machine s -> Int -> [Cursor]
         -> ST s (Maybe Cursor, Answer, Int)
    scan _ steps [] = pure (Nothing, NotFound, steps)
    scan m steps (start : others) = do
      (answer, left, tp) <- go steps 0 start 0 0
      case answer of
        NotFound -> do
          undoTo tp 0
          scan m left others
        _ -> pure (Just start, answer, left)
      where
        -- n: steps left, pc: the next instruction, cur: the place in
        -- the text, sp: ways on the stack, tp: entries on the trail.
        -- Gives back the trail's height, too, to clean up after.
        -- n：残りの手数、pc：次の命令、cur：文字列の中の場所、
        -- sp：スタックの道の数、tp：取り消しの記録の数。あとで片付ける
        -- ために、記録の高さも返す
        go :: Int -> Int -> Cursor -> Int -> Int -> ST s (Answer, Int, Int)
        go n pc cur sp tp
          | n <= 0    = pure (OutOfSteps, 0, tp)
          | otherwise = exec (n - 1) (prog ! pc) pc cur sp tp
-- ANCHOR_END: run

        -- ANCHOR: exec
        -- ⚙️ Run one instruction; n steps are left after it.
        -- 命令を1つ動かす。そのあとの残りの手数は n
        exec n inst pc cur sp tp = case inst of
          Take a -> case step cur of
            Just (x, c) | fitsChar a x -> go n (pc + 1) c sp tp
            _                          -> failed n sp tp
          Assert a
            | holds a cur -> next tp
            | otherwise   -> failed n sp tp
          -- Put the second way on the stack: only where, not a whole
          -- thread.
          -- 2つ目の道をスタックに積む。積むのは場所だけで、スレッドの
          -- 丸ごとではない
          Split a b -> do
            sp' <- push (choices m) sp (Retry b cur tp)
            go n a cur sp' tp
          Jmp a   -> go n a cur sp tp
          Open g  -> setOpen g (Just cur) tp >>= next
          Close g -> do
            o <- MV.read (opensOf m) g
            case o of
              Just from -> setCap g (Just (spanBetween from cur)) tp >>= next
              Nothing   -> failed n sp tp  -- (not reached / 来ない)
          Ref g       -> again stepOver g
          RefNoCase g -> again (stepOverBy sameCase) g
          Keep -> setCap 0 (Just (keptAt cur)) tp >>= next
          Match
            | accept cur -> do
                caps <- frozenCaps
                pure (Found cur caps, n, tp)
            | otherwise -> failed n sp tp
          -- ANCHOR_END: exec
          -- ANCHOR: execmarks
          SetMark k -> setMark k (at cur) tp >>= next
          IfStill k a -> do
            still <- isStill k
            if still then go n a cur sp tp else next tp
          -- ANCHOR_END: execmarks
          -- ANCHOR: execfences
          Fence -> do
            sp' <- push (choices m) sp (Wall Nothing cur tp)
            go n (pc + 1) cur sp' tp
          FenceNot a -> do
            sp' <- push (choices m) sp (Wall (Just a) cur tp)
            go n (pc + 1) cur sp' tp
          -- Only the ways down to the fence go; the trail stays, so a
          -- later going back still puts back what the inside wrote.
          -- 捨てるのは区切りまでの道だけ。取り消しの記録は残すので、
          -- あとで戻るときも、中で書いたものを書き戻せる
          Cut     -> withFence (\_ below -> go n (pc + 1) cur below tp)
          CutBack -> withFence (\w below -> go n (pc + 1) w below tp)
          CutFail -> withFence (\_ below -> failed n below tp)
          -- ANCHOR_END: execfences
          -- ANCHOR: execbehind
          Back d -> case back d cur of
            Just c  -> go n (pc + 1) c sp tp
            Nothing -> failed n sp tp
          AtMark k -> do
            still <- isStill k
            if still then next tp else failed n sp tp
          -- ANCHOR_END: execbehind
          -- ANCHOR: where
          where
            next tp' = go n (pc + 1) cur sp tp'
            isStill k = (== at cur) <$> MU.read (marksOf m) k
            again over g = do
              s <- MV.read (capsOf m) g
              case s >>= \sp_ -> over (spanText sp_) cur of
                Just c  -> go n (pc + 1) c sp tp
                Nothing -> failed n sp tp
            withFence k = do
              found <- fenceBelow sp
              case found of
                Just (w, below) -> k w below
                Nothing         -> failed n sp tp  -- (not reached / 来ない)
          -- ANCHOR_END: where

        -- ANCHOR: failed
        -- ↩️ This way failed: take the next way from the stack, and put
        --    back what was written since it was put there.
        -- この道は失敗。スタックから次の道を取り、それを積んだあとに
        -- 書き換えたものを、書き戻す
        failed :: Int -> Int -> Int -> ST s (Answer, Int, Int)
        failed n 0 tp = pure (NotFound, n, tp)
        failed n sp tp = do
          e <- peek (choices m) (sp - 1)
          case e of
            Retry pc c h -> do
              undoTo tp h
              go n pc c (sp - 1) h
            Wall Nothing _ _ -> failed n (sp - 1) tp
            Wall (Just a) c h -> do
              undoTo tp h
              go n a c (sp - 1) h
        -- ANCHOR_END: failed

        -- ANCHOR: fence
        -- ✂️ The nearest fence: its place, and how many ways are below
        --    it (the fence and the ways above it are thrown away).
        -- いちばん近い区切り：その場所と、その下の道の数（区切りと、
        -- その上の道は捨てる）
        fenceBelow :: Int -> ST s (Maybe (Cursor, Int))
        fenceBelow 0 = pure Nothing
        fenceBelow sp = do
          e <- peek (choices m) (sp - 1)
          case e of
            Wall _ w _ -> pure (Just (w, sp - 1))
            Retry {}   -> fenceBelow (sp - 1)
        -- ANCHOR_END: fence

        -- ANCHOR: write
        -- ✍️ Write over a drawer, and put the old value on the trail.
        --    Gives back the new height of the trail.
        -- 引き出しを書き換え、前の値を記録に積む。記録の新しい高さを返す
        setCap :: Int -> Maybe Span -> Int -> ST s Int
        setCap g new tp = do
          old <- MV.read (capsOf m) g
          MV.write (capsOf m) g new
          push (trail m) tp (UndoCap g old)

        setOpen :: Int -> Maybe Cursor -> Int -> ST s Int
        setOpen g new tp = do
          old <- MV.read (opensOf m) g
          MV.write (opensOf m) g new
          push (trail m) tp (UndoOpen g old)

        setMark :: Int -> Int -> Int -> ST s Int
        setMark k new tp = do
          old <- MU.read (marksOf m) k
          MU.write (marksOf m) k new
          push (trail m) tp (UndoMark k old)

        -- ⏪ Put back the old values, from the top of the trail down to
        --    height h.
        -- 記録のいちばん上から、高さ h まで、前の値を書き戻す
        undoTo :: Int -> Int -> ST s ()
        undoTo tp h
          | tp <= h   = pure ()
          | otherwise = do
              u <- peek (trail m) (tp - 1)
              case u of
                UndoCap  g old -> MV.write (capsOf m)  g old
                UndoOpen g old -> MV.write (opensOf m) g old
                UndoMark k old -> MU.write (marksOf m) k old
              undoTo (tp - 1) h
        -- ANCHOR_END: write

        -- ANCHOR: frozen
        -- 🧊 At a match: the groups, as the table the rest of MiniRegex
        --    knows (the same Caps as the pure VM's).
        -- 一致したとき：グループを、MiniRegex のほかの部分が知っている
        -- 表にする（純粋な VM と同じ Caps）
        frozenCaps :: ST s Caps
        frozenCaps = do
          spans <- MV.foldr' (:) [] (capsOf m)
          pure (IntMap.fromList [(g, s) | (g, Just s) <- zip [0 ..] spans])
        -- ANCHOR_END: frozen
-- ANCHOR: fresh
-- 🐢 The first way this chapter wrote it: a new machine for each place
--    (runST, the arrays, the stacks, and counting the drawers, again
--    and again). The same answers, but slow; kept to measure with.
-- この章で最初に書いた形：場所ごとに、新しい機械を作る（runST、配列、
-- スタック、引き出しを数えるのも、毎回）。答えは同じだが、遅い。
-- 測りくらべるために残す
scanVMSTFresh
  :: Program -> Int -> (Cursor -> Bool) -> [Cursor]
  -> (Maybe Cursor, Answer, Int)
scanVMSTFresh _ n _ [] = (Nothing, NotFound, n)
scanVMSTFresh prog n accept (c : cs) = case scanVMST prog n accept [c] of
  (_, NotFound, left) -> scanVMSTFresh prog left accept cs
  other               -> other
-- ANCHOR_END: fresh
-- ANCHOR_END: all
