-- ANCHOR: all
-- | The backtracking VM again, written with ST. It runs the same
--   program, one instruction a step, the same as MiniRegex.VM; only
--   what it keeps is different. The groups and the marks are mutable
--   arrays, one of each, written over in place. Each time it writes
--   over one, it puts the old value on the trail; going back to a way
--   to try, it puts back the old values, down to where the trail was.
-- バックトラックの VM を、ST でもう一度。MiniRegex.VM と同じ
-- プログラムを、同じく1命令1ステップで動かす。ちがうのは、持ち物だけ。
-- グループと印は、書き換えられる配列を1つずつ持ち、その場で書き換える。
-- 書き換えるたびに、前の値を「取り消しの記録」に積む。ほかの試し方へ
-- 戻るときは、そのときの記録の高さまで、前の値を書き戻す
module MiniRegex.STVM
  ( scanVMST
  , scanVMSTUntil
  , scanVMSTEach
  , scanVMSTFresh
  , Choice (..)
  , Called (..)
  , Undo (..)
  , slotsOf
  ) where

-- ANCHOR: imports
import Control.Monad.ST (ST, runST)
import Data.Array ((!))
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (listToMaybe)
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed.Mutable qualified as MU

import MiniRegex.Caps (Caps, keptAt, spanBetween)
import MiniRegex.Cursor hiding (before, rest)
import MiniRegex.Grapheme (clusterEnd)
import MiniRegex.Program
import MiniRegex.Rules (eatAll, fitsChar, holds)
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
  = Retry !Int !Cursor !Int ![Called]
  | Wall !(Maybe Int) !Cursor !Int ![Called]

-- 📞 One call: where to come back, which group was called and where in
--    the text, and how high the trail was then. A list that is never
--    written over: a way to try keeps the calls it was in, as they
--    were (the list is shared, not copied).
-- 呼び出し1つ：戻ってくる番地、どのグループを文字列のどこで呼んだか、
-- そのときの記録の高さ。書き換えないリスト。あとで試す道は、そのときの
-- 呼び出しを、そのまま持つ（リストは分け合うので、写さない）
data Called = Called
  { backTo       :: !Int
  , calledGroup  :: !Int
  , calledPlace  :: !Int
  , calledHeight :: !Int
  }

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
-- scanVM と同じ型：場所を1つずつ、ステップ数を分け合いながら動かす。機械の
-- 持ち物は、どの場所にも使えるように1度だけ作る。場所と場所のあいだは、
-- 取り消しの記録が全部を元に戻す。書き換えていることは、外からは
-- わからない。runST が閉じ込める
scanVMST
  :: Program -> Int -> (Cursor -> Bool) -> [Cursor]
  -> (Maybe Cursor, Answer, Int)
scanVMST prog limit accept places =
  case scanVMSTUntil prog limit accept places of
    Left rest    -> (listToMaybe rest, OutOfSteps, 0)
    Right answer -> answer
-- ANCHOR_END: run

-- ANCHOR: until
-- ⏸️ The same, but when the steps run out, give back the places not yet
--    done: the one it ran out at, and the ones after it. So whoever
--    goes on (the Pike VM) does not have to start again from the
--    first place, and nobody holds on to the places already done.
-- 同じだが、ステップ数が尽きたら、まだ済んでいない場所を返す：尽きた場所と、
-- そのあとの場所。続きを引き受ける人（Pike VM）は、最初の場所から
-- やり直さなくてよい。済んだ場所を、だれも持ち続けない
scanVMSTUntil
  :: Program -> Int -> (Cursor -> Bool) -> [Cursor]
  -> Either [Cursor] (Maybe Cursor, Answer, Int)
scanVMSTUntil = scanVMSTEach maxBound
-- ANCHOR_END: until

-- ANCHOR: each
-- 🪙 The same, but each place gets at most cap steps of its own (and
--    no more than are left). A place that uses them all up stops the
--    run there, as if the steps had run out.
-- 同じだが、どの場所にも、その場所だけのステップ数を cap まで渡す（残りより
-- 多くは渡さない）。使い切った場所があれば、ステップ数が尽きたときと同じく、
-- そこで止める
scanVMSTEach
  :: Int -> Program -> Int -> (Cursor -> Bool) -> [Cursor]
  -> Either [Cursor] (Maybe Cursor, Answer, Int)
scanVMSTEach cap prog limit accept places = runST $ do
  m <- newMachine (slotsOf prog)
  scan m limit places
  where
    scan :: forall s. Machine s -> Int -> [Cursor]
         -> ST s (Either [Cursor] (Maybe Cursor, Answer, Int))
    scan _ steps [] = pure (Right (Nothing, NotFound, steps))
    scan m steps here@(start : others) = do
      let given = min cap steps
      (answer, left, tp) <- go given 0 start 0 0 []
      let rest = steps - (given - left)
      case answer of
        NotFound -> do
          undoTo tp 0
          scan m rest others
        OutOfSteps -> pure (Left here)
        _ -> pure (Right (Just start, answer, rest))
      where
        -- n: steps left, pc: the next instruction, cur: the place in
        -- the text, sp: ways on the stack, tp: entries on the trail,
        -- cs: the calls we are in. Gives back the trail's height, too,
        -- to clean up after.
        -- n：残りのステップ数、pc：次の命令、cur：文字列の中の場所、
        -- sp：スタックの道の数、tp：取り消しの記録の数、cs：いま中に
        -- いる呼び出し。あとで片付けるために、記録の高さも返す
        go :: Int -> Int -> Cursor -> Int -> Int -> [Called]
           -> ST s (Answer, Int, Int)
        go n pc cur sp tp cs
          | n <= 0    = pure (OutOfSteps, 0, tp)
          | otherwise = exec (n - 1) (prog ! pc) pc cur sp tp cs
-- ANCHOR_END: each

        -- ANCHOR: exec
        -- ⚙️ Run one instruction; n steps are left after it.
        -- 命令を1つ動かす。そのあとの残りのステップ数は n
        exec n inst pc cur sp tp cs = case inst of
          Take a -> case step cur of
            Just (x, c) | fitsChar a x -> go n (pc + 1) c sp tp cs
            _                          -> failed n sp tp
          Assert a
            | holds a cur -> next tp
            | otherwise   -> failed n sp tp
          -- Put the second way on the stack: only where, not a whole
          -- thread.
          -- 2つ目の道をスタックに積む。積むのは場所だけで、スレッドの
          -- 丸ごとではない
          Split a b -> do
            sp' <- push (choices m) sp (Retry b cur tp cs)
            go n a cur sp' tp cs
          Jmp a   -> go n a cur sp tp cs
          Open g  -> setOpen g (Just cur) tp >>= next
          Close g -> do
            o <- MV.read (opensOf m) g
            case o of
              Just from -> setCap g (Just (spanBetween from cur)) tp >>= next
              Nothing   -> failed n sp tp  -- (not reached / 来ない)
          Ref g       -> again stepOver g
          RefNoCase g -> again (stepOverBy sameCase) g
          -- ANCHOR: cluster
          TakeCluster -> case clusterEnd cur of
            Just c  -> go n (pc + 1) c sp tp cs
            Nothing -> failed n sp tp
          -- ANCHOR_END: cluster
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
            if still then go n a cur sp tp cs else next tp
          -- ANCHOR_END: execmarks
          -- ANCHOR: execfences
          Fence -> do
            sp' <- push (choices m) sp (Wall Nothing cur tp cs)
            go n (pc + 1) cur sp' tp cs
          FenceNot a -> do
            sp' <- push (choices m) sp (Wall (Just a) cur tp cs)
            go n (pc + 1) cur sp' tp cs
          -- Only the ways down to the fence go; the trail stays, so a
          -- later going back still puts back what the inside wrote.
          -- 捨てるのは区切りまでの道だけ。取り消しの記録は残すので、
          -- あとで戻るときも、中で書いたものを書き戻せる
          Cut     -> withFence (\_ below -> go n (pc + 1) cur below tp cs)
          CutBack -> withFence (\w below -> go n (pc + 1) w below tp cs)
          CutFail -> withFence (\_ below -> failed n below tp)
          -- ANCHOR_END: execfences
          -- ANCHOR: execbehind
          Back d -> case back d cur of
            Just c  -> go n (pc + 1) c sp tp cs
            Nothing -> failed n sp tp
          AtMark k -> do
            still <- isStill k
            if still then next tp else failed n sp tp
          -- ANCHOR_END: execbehind
          -- ANCHOR: execcall
          Gosub g a
            | looping g -> pure (Looped g (at cur), n, tp)
            | otherwise ->
                go n a cur sp tp (Called (pc + 1) g (at cur) tp : cs)
          Return -> case cs of
            f : outer -> do
              tp' <- rewind tp (calledHeight f)
              go n (backTo f) cur sp tp' outer
            [] -> failed n sp tp  -- (not reached / 来ない)
          -- ANCHOR_END: execcall
          -- ANCHOR: exectakeall
          -- Eat them all; nothing goes on the stack.
          -- 全部食べる。スタックには何も積まない
          TakeAll a -> case eatAll a n cur of
            (c, n') | n' > 0    -> go n' (pc + 1) c sp tp cs
                    | otherwise -> pure (OutOfSteps, 0, tp)
          -- ANCHOR_END: exectakeall
          -- ANCHOR: where
          where
            next tp' = go n (pc + 1) cur sp tp' cs
            isStill k = (== at cur) <$> MU.read (marksOf m) k
            again over g = do
              s <- MV.read (capsOf m) g
              case s >>= \sp_ -> over (spanText sp_) cur of
                Just c  -> go n (pc + 1) c sp tp cs
                Nothing -> failed n sp tp
            -- the latest call of the same group: at the same place?
            -- 同じグループのいちばん新しい呼び出しは、同じ場所だったか
            looping g = case [f | f <- cs, calledGroup f == g] of
              f : _ -> calledPlace f == at cur
              []    -> False
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
            Retry pc c h cs -> do
              undoTo tp h
              go n pc c (sp - 1) h cs
            Wall Nothing _ _ _ -> failed n (sp - 1) tp
            Wall (Just a) c h cs -> do
              undoTo tp h
              go n a c (sp - 1) h cs
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
            Wall _ w _ _ -> pure (Just (w, sp - 1))
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

        -- ANCHOR: rewind
        -- ↩️ Back from a call: the groups, the open groups and the
        --    marks as they were at the call. The trail has the old
        --    values since then: write them again, from the top down to
        --    h, but as new writes, put on the trail too. So, going back
        --    into the call later, undoTo still finds the call's own
        --    values. \K (drawer 0) stays as it is.
        -- 呼び出しから戻ったとき：グループ・開いたグループ・印を、呼んだ
        -- ときの値に。記録には、それからの前の値がある。それを、上から
        -- 高さ h まで、もう一度書く。ただし、新しい書き換えとして記録にも
        -- 積む。こうすれば、あとで呼び出しの中へ戻るときも、undoTo が
        -- 中の値を見つけられる。\K（0番の引き出し）は、そのまま
        rewind :: Int -> Int -> ST s Int
        rewind tp h = again (tp - 1) tp
          where
            again i top
              | i < h     = pure top
              | otherwise = do
                  u <- peek (trail m) i
                  top' <- case u of
                    UndoCap 0 _    -> pure top
                    UndoCap g old  -> setCap g old top
                    UndoOpen g old -> setOpen g old top
                    UndoMark k old -> setMark k old top
                  again (i - 1) top'
        -- ANCHOR_END: rewind

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
