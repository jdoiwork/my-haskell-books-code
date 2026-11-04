-- ANCHOR: all
-- 💰 The guild's treasury, shared by many threads.
-- たくさんのスレッドで分け合う、ギルドの金庫
module Treasury
  ( countWithIORef
  , countWithAtomic
  , transfer
  , payDay
  ) where

import Control.Concurrent.Async (forConcurrently_, replicateConcurrently_)
import Control.Concurrent.STM
import Control.Monad            (replicateM_, when)
import Data.IORef

-- ANCHOR: ioref
-- 🐞 Ten clerks each add 1 to the count 100,000 times, at the same time.
-- 10人の係が、それぞれ 100,000回、同時に数を1増やす
countWithIORef :: IO Int
countWithIORef = do
  count <- newIORef 0
  replicateConcurrently_ 10 $
    replicateM_ 100000 (modifyIORef' count (+ 1))
  readIORef count

-- ✅ The same, with an atomic update: read and write as one step.
-- 同じものを、不可分な更新で：読むと書くを、ひと続きの1歩に
countWithAtomic :: IO Int
countWithAtomic = do
  count <- newIORef 0
  replicateConcurrently_ 10 $
    replicateM_ 100000 (atomicModifyIORef' count (\n -> (n + 1, ())))
  readIORef count
-- ANCHOR_END: ioref

-- ANCHOR: transfer
-- 💸 Move gold from one purse to another, all or nothing.
--    If there is not enough, wait (retry) until there is.
-- 財布から財布へ、金貨を移す（全部か、無しか）。
-- 足りなければ、足りるようになるまで待つ（retry）
transfer :: TVar Int -> TVar Int -> Int -> STM ()
transfer from to amount = do
  balance <- readTVar from
  when (balance < amount) retry
  modifyTVar' from (subtract amount)
  modifyTVar' to   (+ amount)
-- ANCHOR_END: transfer

-- ANCHOR: payDay
-- 🎉 Pay day: the guild pays 10 gold to each of 100 adventurers,
--    all at the same time. Returns (the guild, all the purses).
-- 給料日：ギルドが100人の冒険者に、同時に10枚ずつ払う。
-- （ギルドの残り、全員の財布の合計）を返す
payDay :: IO (Int, Int)
payDay = do
  guild  <- newTVarIO 1000
  purses <- mapM (const (newTVarIO 0)) [1 .. 100 :: Int]
  forConcurrently_ purses $ \purse ->
    atomically (transfer guild purse 10)
  left  <- readTVarIO guild
  paid  <- sum <$> mapM readTVarIO purses
  pure (left, paid)
-- ANCHOR_END: payDay
-- ANCHOR_END: all
