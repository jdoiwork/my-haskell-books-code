-- ANCHOR: all
-- | A stack that grows: a mutable array, and how many items are on it.
--   The count is not kept inside; whoever uses the stack carries it
--   (the VM keeps it as an argument of its loop, next to the program
--   counter). When the array is full, a twice as long one takes its
--   place.
-- 伸びるスタック：書き換えられる配列と、積んである数。数は中に持たず、
-- 使う側が持ち歩く（VM は、命令の番地と並べて、ループの引数に持つ）。
-- 配列がいっぱいになったら、2倍の長さの配列に取り替える
module MiniRegex.Stack
  ( Stack
  , newStack
  , push
  , peek
  ) where

-- ANCHOR: imports
import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector.Mutable qualified as MV
-- ANCHOR_END: imports

-- ANCHOR: stack
-- 🥞 The array, in a box: growing puts a new array in the box.
-- 箱に入れた配列。伸ばすときは、箱に新しい配列を入れる
newtype Stack s a = Stack (STRef s (MV.MVector s a))

-- 🆕 An empty stack, with room for 16 to start with.
-- 空のスタック。はじめは 16 個ぶんの場所
newStack :: ST s (Stack s a)
newStack = do
  v <- MV.new 16
  Stack <$> newSTRef v

-- ⬆️ Put x on a stack that has n items; gives back n + 1.
-- n 個積んであるスタックに x を積む。n + 1 を返す
push :: Stack s a -> Int -> a -> ST s Int
push (Stack ref) n x = do
  v <- readSTRef ref
  v' <- if n < MV.length v
          then pure v
          else do
            -- Full: twice as long, the old items copied over.
            -- いっぱい：2倍の長さにして、前の中身を写す
            w <- MV.grow v (MV.length v)
            writeSTRef ref w
            pure w
  MV.write v' n x
  pure (n + 1)

-- 👀 The item at place i (0 is the bottom).
-- i 番目（0 がいちばん下）の中身
peek :: Stack s a -> Int -> ST s a
peek (Stack ref) i = do
  v <- readSTRef ref
  MV.read v i
-- ANCHOR_END: stack
-- ANCHOR_END: all
