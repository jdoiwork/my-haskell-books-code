-- ANCHOR: all
-- 🔒 Two purses, each with its own lock (an MVar).
--    Written with MonadConc, so that dejafu can try every order.
-- 錠（MVar）の付いた、2つの財布。
-- dejafu がすべての順番を試せるように、MonadConc で書く
module Locks (lockedTransfer, swapGifts, swapGiftsSafe) where

import Control.Concurrent.Classy

-- ANCHOR: locked
-- 🔒 Lock the first purse, then the second, and move the gold.
-- 1つ目の財布、2つ目の財布の順に錠をかけて、金貨を移す
lockedTransfer :: MonadConc m => MVar m Int -> MVar m Int -> Int -> m ()
lockedTransfer from to amount = do
  a <- takeMVar from
  b <- takeMVar to
  putMVar to   (b + amount)
  putMVar from (a - amount)
-- ANCHOR_END: locked

-- ANCHOR: swap
-- 🎁 Lambda and Python send gifts to each other, at the same time.
-- Lambda と Python が、同時に、おたがいに贈り物をする
swapGifts :: MonadConc m => m (Int, Int)
swapGifts = do
  lambda <- newMVar 100
  python <- newMVar 100
  done   <- newEmptyMVar
  _ <- fork (lockedTransfer lambda python 10 >> putMVar done ())
  _ <- fork (lockedTransfer python lambda 20 >> putMVar done ())
  takeMVar done
  takeMVar done
  (,) <$> readMVar lambda <*> readMVar python
-- ANCHOR_END: swap

-- ANCHOR: safe
-- ✅ The fix: always lock Lambda's purse first, whoever pays.
-- 直し方：誰が払うときも、いつも Lambda の財布から錠をかける
swapGiftsSafe :: MonadConc m => m (Int, Int)
swapGiftsSafe = do
  lambda <- newMVar 100
  python <- newMVar 100
  done   <- newEmptyMVar
  _ <- fork (lockedTransfer lambda python 10 >> putMVar done ())
  -- Python pays 20 = Lambda pays -20: Lambda's purse is locked first.
  -- Python が 20 払う = Lambda が -20 払う（Lambda の財布から錠をかける）
  _ <- fork (lockedTransfer lambda python (-20) >> putMVar done ())
  takeMVar done
  takeMVar done
  (,) <$> readMVar lambda <*> readMVar python
-- ANCHOR_END: safe
-- ANCHOR_END: all
