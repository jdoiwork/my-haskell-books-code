-- ANCHOR: all
-- | 🧊 Freezing and thawing vectors, and code that runs in both IO and ST.
-- Vector を凍らせる・溶かす。IO でも ST でも動くコード
module Frozen
  ( freezeDemo
  , hitBoss
  , fillSquares
  , squares
  , squaresIO
  , buildSafe
  , buildUnsafe
  ) where

-- ANCHOR: imports
-- The loop, and PrimMonad (the monads that can rewrite memory).
-- ループと、PrimMonad（メモリーを書き換えられるモナド）
import Control.Monad           (forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST        (runST)

-- Vectors.
-- Vector
import Data.Vector.Unboxed         qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU
-- ANCHOR_END: imports

-- ANCHOR: demo
-- 🧪 Freeze a vector twice, then rewrite the original.
-- Vector を2通りに凍らせてから、元の Vector を書き換える
freezeDemo :: IO (Int, Int)
freezeDemo = do
  hps    <- MVU.replicate 3 (50 :: Int)
  copied <- VU.freeze hps
  shared <- VU.unsafeFreeze hps
  MVU.write hps 0 0
  pure (copied VU.! 0, shared VU.! 0)
-- ANCHOR_END: demo

-- ANCHOR: modify
-- 🐉 Hit only the boss (slot 0). VU.modify copies the vector once,
--    lets us rewrite the copy, and freezes it again.
-- ボス（0番）にだけダメージ。VU.modify は Vector を1回コピーし、
-- そのコピーを書き換えさせてから、また凍らせる
hitBoss :: Int -> VU.Vector Int -> VU.Vector Int
hitBoss damage hps =
  VU.modify (\v -> MVU.modify v (subtract damage) 0) hps
-- ANCHOR_END: modify

-- ANCHOR: prim
-- 🔢 Write 0, 1, 4, 9, ... into a mutable vector.
--    It works in any monad that can rewrite memory: IO and ST.
-- 書き換えられる Vector に 0, 1, 4, 9, ... を書く。
-- メモリーを書き換えられるモナドなら、どれでも動く（IO でも ST でも）
-- ANCHOR: primsig
fillSquares :: PrimMonad m => MVU.MVector (PrimState m) Int -> m ()
-- ANCHOR_END: primsig
fillSquares v =
  forM_ [0 .. MVU.length v - 1] $ \i ->
    MVU.write v i (i * i)

-- 🧊 In ST: VU.create runs the steps and freezes the result.
-- ST で使う。VU.create は手順を動かして、結果を凍らせる
squares :: Int -> VU.Vector Int
squares n = VU.create $ do
  v <- MVU.new n
  fillSquares v
  pure v

-- 💻 In IO: the very same fillSquares.
-- IO で使う。まったく同じ fillSquares を
squaresIO :: Int -> IO (VU.Vector Int)
squaresIO n = do
  v <- MVU.new n
  fillSquares v
  VU.freeze v
-- ANCHOR_END: prim

-- ANCHOR: build
-- 📋 Build a vector of n numbers, then freeze it with a copy...
-- n 個の数の Vector を作って、コピーして凍らせる……
buildSafe :: Int -> VU.Vector Int
buildSafe n = runST $ do
  v <- MVU.new n
  forM_ [0 .. n - 1] $ \i -> MVU.write v i i
  VU.freeze v

-- ...or without one.
-- ……コピーせずに凍らせる
buildUnsafe :: Int -> VU.Vector Int
buildUnsafe n = runST $ do
  v <- MVU.new n
  forM_ [0 .. n - 1] $ \i -> MVU.write v i i
  VU.unsafeFreeze v
-- ANCHOR_END: build
-- ANCHOR_END: all
