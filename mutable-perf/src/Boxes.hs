-- ANCHOR: all
-- | 📦 Boxes you can rewrite: IORef, STRef, and a one-slot vector.
-- 書き換えられる箱：IORef、STRef、1マスだけの Vector
module Boxes
  ( countLazy
  , countStrict
  , countSTLazy
  , countSTStrict
  , countAtomic
  , countSlot
  , sumState
  , sumST
  , sumFold
  ) where

-- ANCHOR: imports
-- Loops, ST and State.
-- ループ、ST、State
import Control.Monad              (forM_)
import Control.Monad.ST           (runST)
import Control.Monad.State.Strict (execState, modify')

-- The boxes.
-- 箱
import Data.IORef
import Data.STRef
import Data.Vector.Unboxed.Mutable qualified as MVU
-- ANCHOR_END: imports

-- ANCHOR: ioref
-- 🐌 Count to n in an IORef, with modifyIORef (no ').
-- IORef で n まで数える。modifyIORef（' なし）で
countLazy :: Int -> IO Int
countLazy n = do
  box <- newIORef 0
  forM_ [1 .. n] $ \_ ->
    modifyIORef box (+ 1)
  readIORef box

-- 🏃 The same, with modifyIORef'.
-- 同じことを、modifyIORef' で
countStrict :: Int -> IO Int
countStrict n = do
  box <- newIORef 0
  forM_ [1 .. n] $ \_ ->
    modifyIORef' box (+ 1)
  readIORef box
-- ANCHOR_END: ioref

-- ANCHOR: stref
-- 🐌 The same two, with an STRef inside runST: pure functions.
-- 同じ2つを、runST の中の STRef で。外から見れば純粋な関数
countSTLazy :: Int -> Int
countSTLazy n = runST $ do
  box <- newSTRef 0
  forM_ [1 .. n] $ \_ ->
    modifySTRef box (+ 1)
  readSTRef box

-- 🏃 With modifySTRef'.
-- modifySTRef' で
countSTStrict :: Int -> Int
countSTStrict n = runST $ do
  box <- newSTRef 0
  forM_ [1 .. n] $ \_ ->
    modifySTRef' box (+ 1)
  readSTRef box
-- ANCHOR_END: stref

-- ANCHOR: atomic
-- 🔒 With atomicModifyIORef': safe even when many threads count.
-- atomicModifyIORef' で。いくつものスレッドで数えても安全
countAtomic :: Int -> IO Int
countAtomic n = do
  box <- newIORef 0
  forM_ [1 .. n] $ \_ ->
    atomicModifyIORef' box (\x -> (x + 1, ()))
  readIORef box
-- ANCHOR_END: atomic

-- ANCHOR: slot
-- 🎰 A vector with one slot, used as an unboxed box.
-- 1マスだけの Vector を、箱に入れない箱として使う
countSlot :: Int -> IO Int
countSlot n = do
  box <- MVU.replicate 1 0
  forM_ [1 .. n] $ \_ ->
    MVU.unsafeModify box (+ 1) 0
  MVU.read box 0
-- ANCHOR_END: slot

-- ANCHOR: sums
-- 📦 Sum with State: the total is passed from step to step.
-- State で合計する。合計は、手順から手順へ渡される
sumState :: [Int] -> Int
sumState xs = execState (mapM_ (\x -> modify' (+ x)) xs) 0

-- 🔧 Sum with ST: a real box, rewritten in place.
-- ST で合計する。本物の箱を、その場で書き換える
sumST :: [Int] -> Int
sumST xs = runST $ do
  total <- newSTRef 0
  mapM_ (\x -> modifySTRef' total (+ x)) xs
  readSTRef total

-- ➕ Sum with a strict fold, no box at all.
-- 箱を使わず、正格な畳み込みで合計する
sumFold :: [Int] -> Int
sumFold xs = foldl' (+) 0 xs
-- ANCHOR_END: sums
-- ANCHOR_END: all
