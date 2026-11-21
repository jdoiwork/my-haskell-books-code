-- ANCHOR: all
-- | The backtracking VM that remembers where it has been: the bounded
--   backtracker of Rust's regex-automata and the "bitstate" of Go's
--   regexp, for MiniRegex. It runs the same program as MiniRegex.STVM,
--   but keeps a table of bits, one for each (instruction, place). The
--   first time it comes to a pair, it sets the bit; the second time, it
--   knows that everything after it failed before, and fails at once.
--   Each pair is tried once, so it takes linear time; but the table
--   grows with the text, so it takes only short texts.
-- 通った所を覚えておくバックトラックの VM：Rust の regex-automata の
-- 上限つきのバックトラックと、Go の regexp の bitstate の MiniRegex 版。
-- MiniRegex.STVM と同じプログラムを動かすが、（命令、場所）の組ごとに
-- 1ビットの表を持つ。初めて着いた組には、ビットを立てる。2度目に
-- 着いたら、その先は前に全部だめだったとわかっているので、すぐ失敗する。
-- どの組も1度しか試さないので線形時間。ただし、表が文字列とともに
-- 大きくなるので、短い文字列しか受け取らない
module MiniRegex.Bounded
  ( scanBounded
  , scanBoundedAtTheDoor
  , canBound
  , boundedFits
  , visitedCapacity
  ) where

-- ANCHOR: imports
import Control.Monad.ST (ST, runST)
import Data.Array.Base (unsafeAt)
import Data.Bits (setBit, shiftR, testBit, (.&.))
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (isNothing)
import Data.Text.Unsafe (lengthWord8)
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed.Mutable qualified as MU
import Data.Word (Word64)

import MiniRegex.Caps (Caps, keptAt, spanBetween)
import MiniRegex.Cursor hiding (before, rest)
import MiniRegex.Pike (blocker)
import MiniRegex.Program
import MiniRegex.Rules (eatAll, fitsChar, holds)
import MiniRegex.STVM (Choice (..), Undo (..), slotsOf)
import MiniRegex.Stack
import MiniRegex.Types (Span (..))
import MiniRegex.VM (Answer (..))
-- ANCHOR_END: imports

-- ANCHOR: fits
-- 📏 How many bits the table may have: 256 KiB, the same as the
--    default of Rust's BoundedBacktracker (visited_capacity).
-- 表のビットの数の上限：256 KiB。Rust の BoundedBacktracker の既定
-- （visited_capacity）と同じ
visitedCapacity :: Int
visitedCapacity = 256 * 1024 * 8

-- 🧩 Programs it can run: the ones the Pike VM can run, and without
--    marks. (A mark makes the same instruction at the same place lead
--    to different things, so a bit for each pair is not enough.)
-- 動かせるプログラム：Pike VM が動かせて、印のないもの（印があると、
-- 同じ場所の同じ命令でも、その先が変わる。組ごとの1ビットでは足りない）
canBound :: Program -> Bool
canBound prog =
  isNothing (blocker prog) && not (any isMark (instructions prog))
  where
    isMark i = case i of
      SetMark _   -> True
      IfStill _ _ -> True
      _           -> False

-- 📐 Does the table fit, from this place to the end of the text? One
--    bit for each instruction, at each byte and after the last one.
-- この場所から文字列の終わりまでで、表が収まるか。命令ごとに、どの
-- バイトにも、最後のバイトのうしろにも1ビット
boundedFits :: Program -> Cursor -> Bool
boundedFits prog c = size * width <= visitedCapacity
  where
    size  = length (instructions prog)
    width = lengthWord8 (whole c) - byte c + 1
-- ANCHOR_END: fits

-- ANCHOR: machine
-- 🏭 The machine's things: the table, the groups, the starts of the
--    open groups, and the two stacks of the VM with ST.
-- 機械の持ち物：表、グループ、開いたグループの始まり、ST の VM と
-- 同じ2つのスタック
data Machine s = Machine
  { seen     :: !(MU.MVector s Word64)
  , capsOf   :: !(MV.MVector s (Maybe Span))
  , opensOf  :: !(MV.MVector s (Maybe Cursor))
  , choices  :: !(Stack s Choice)
  , trail    :: !(Stack s Undo)
  }
-- ANCHOR_END: machine

-- ANCHOR: run
-- 🏃 The same type as scanVMST. The table is made once, for all the
--    places: a pair that failed from one place fails from the next one,
--    too (what comes after it does not look back at where it started).
--    The caller makes sure that canBound and boundedFits hold.
-- scanVMST と同じ型。表は、どの場所にも使えるように1度だけ作る。ある
-- 場所から試してだめだった組は、次の場所から試してもだめ（その先は、
-- どこから始めたかを見ない）。canBound と boundedFits は、呼ぶ側が
-- 確かめておく
scanBounded, scanBoundedAtTheDoor
  :: Program -> Int -> (Cursor -> Bool) -> [Cursor]
  -> (Maybe Cursor, Answer, Int)
scanBounded          = scanBoundedBy True
scanBoundedAtTheDoor = scanBoundedBy False

-- 🔀 eachChar: does takeall look at the table at each character, or
--    only at its door?
-- eachChar：takeall が、1文字ごとに表を見るか、入り口でだけ見るか
scanBoundedBy
  :: Bool -> Program -> Int -> (Cursor -> Bool) -> [Cursor]
  -> (Maybe Cursor, Answer, Int)
scanBoundedBy _ _ limit _ [] = (Nothing, NotFound, limit)
scanBoundedBy eachChar prog limit accept places@(first : _) = runST $ do
  let (groups, _) = slotsOf prog
  m <- Machine <$> MU.replicate cells 0
               <*> MV.replicate groups Nothing
               <*> MV.replicate groups Nothing
               <*> newStack
               <*> newStack
  scan m limit places
  where
    size  = length (instructions prog)
    width = lengthWord8 (whole first) - byte first + 1
    cells = (size * width + 63) `div` 64

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
-- ANCHOR_END: run
        -- ANCHOR: go
        -- The one new thing: before running an instruction, look at
        -- its bit. Set: we were here before, and it failed.
        -- 新しいのは1つだけ：命令を動かす前に、そのビットを見る。立って
        -- いれば、前にここへ来て、だめだった
        go :: Int -> Int -> Cursor -> Int -> Int
           -> ST s (Answer, Int, Int)
        go n pc cur sp tp
          | n <= 0    = pure (OutOfSteps, 0, tp)
          | otherwise = do
              new <- firstTime pc cur
              if new
                then exec (n - 1) (prog `unsafeAt` pc) pc cur sp tp
                else failed (n - 1) sp tp

        -- 🔖 Is this the first time at (pc, cur)? Set its bit.
        -- （pc、cur）に来たのは初めてか。ビットを立てる
        firstTime :: Int -> Cursor -> ST s Bool
        firstTime pc cur = do
          let i = pc * width + (byte cur - byte first)
              w = i `shiftR` 6
              b = i .&. 63
          bits <- MU.unsafeRead (seen m) w
          if testBit bits b
            then pure False
            else do
              MU.unsafeWrite (seen m) w (setBit bits b)
              pure True
        -- ANCHOR_END: go

        -- ANCHOR: exec
        -- ⚙️ The instructions the Pike VM runs, the same as in scanVMST
        --    (takeall comes last).
        -- Pike VM が動かす命令。scanVMST と同じ（takeall は最後に）
        exec n inst pc cur sp tp = case inst of
          Take a -> case step cur of
            Just (x, c) | fitsChar a x -> go n (pc + 1) c sp tp
            _                          -> failed n sp tp
          Assert a
            | holds a cur -> next tp
            | otherwise   -> failed n sp tp
          Split a b -> do
            sp' <- push (choices m) sp (Retry b cur tp [])
            go n a cur sp' tp
          Jmp a   -> go n a cur sp tp
          Open g  -> do
            old <- MV.read (opensOf m) g
            MV.write (opensOf m) g (Just cur)
            push (trail m) tp (UndoOpen g old) >>= next
          Close g -> do
            o <- MV.read (opensOf m) g
            case o of
              Just from -> setCap g (spanBetween from cur) tp >>= next
              Nothing   -> failed n sp tp  -- (not reached / 来ない)
          Keep -> setCap 0 (keptAt cur) tp >>= next
          Match
            | accept cur -> do
                caps <- frozenCaps
                pure (Found cur caps, n, tp)
            | otherwise -> failed n sp tp
          -- ANCHOR_END: exec
          -- ANCHOR: takeall
          -- One character at a time, looking at the bit of (pc, place)
          -- each time. Another takeall that passed here before ate
          -- just as far from here on, and failed after it. (Watching
          -- only at the door, as scanVMST eats, is kept to measure.)
          -- 1文字ずつ、そのたびに（pc、場所）のビットを見る。前にここを
          -- 通った takeall は、ここから先も同じだけ食べて、その先で
          -- だめだった（scanVMST と同じく、入り口だけで見張る食べ方も、
          -- 測りくらべるために残す）
          TakeAll a
            | eachChar  -> eat n cur
            | otherwise -> case eatAll a n cur of
                (c, n') | n' > 0    -> go n' (pc + 1) c sp tp
                        | otherwise -> pure (OutOfSteps, 0, tp)
            where
              eat k c
                | k <= 0    = pure (OutOfSteps, 0, tp)
                | otherwise = case step c of
                    Just (x, c') | fitsChar a x -> do
                      new <- firstTime pc c'
                      if new then eat (k - 1) c' else failed (k - 1) sp tp
                    _ -> go k (pc + 1) c sp tp
          -- ANCHOR_END: takeall
          -- ANCHOR: rest
          other -> error ("scanBounded cannot run " ++ show (() <$ other))
          where
            next tp' = go n (pc + 1) cur sp tp'

        -- ↩️ The same as in scanVMST.
        -- scanVMST と同じ
        failed :: Int -> Int -> Int -> ST s (Answer, Int, Int)
        failed n 0 tp = pure (NotFound, n, tp)
        failed n sp tp = do
          e <- peek (choices m) (sp - 1)
          case e of
            Retry pc c h _ -> do
              undoTo tp h
              go n pc c (sp - 1) h
            Wall {} -> failed n (sp - 1) tp  -- (not reached / 来ない)

        setCap :: Int -> Span -> Int -> ST s Int
        setCap g new tp = do
          old <- MV.read (capsOf m) g
          MV.write (capsOf m) g (Just new)
          push (trail m) tp (UndoCap g old)

        undoTo :: Int -> Int -> ST s ()
        undoTo tp h
          | tp <= h   = pure ()
          | otherwise = do
              u <- peek (trail m) (tp - 1)
              case u of
                UndoCap  g old -> MV.write (capsOf m)  g old
                UndoOpen g old -> MV.write (opensOf m) g old
                UndoMark _ _   -> pure ()  -- (not reached / 来ない)
              undoTo (tp - 1) h

        frozenCaps :: ST s Caps
        frozenCaps = do
          spans <- MV.foldr' (:) [] (capsOf m)
          pure (IntMap.fromList [(g, s) | (g, Just s) <- zip [0 ..] spans])
-- ANCHOR_END: rest
-- ANCHOR_END: all
