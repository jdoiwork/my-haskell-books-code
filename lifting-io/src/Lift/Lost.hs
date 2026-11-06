-- ANCHOR: all
-- | Why StateT cannot be MonadUnliftIO: run two StateT steps at the same
--   time, and you get two notebooks back. One of them has to go.
-- StateT が MonadUnliftIO になれない理由：StateT の手順を2つ同時に動かすと、
-- メモ帳が2冊戻ってくる。片方は捨てるしかない
module Lift.Lost
  ( bothAtOnce
  , lostGold
  , savedGold
  ) where

import Control.Concurrent.Async (concurrently)
import Control.Monad.State.Strict

-- ANCHOR: lost
-- 🙈 "If StateT could be unlifted", written by hand. Each step gets a
--    copy of the notebook; we can only keep one copy at the end.
-- 「もし StateT を unlift できたら」を、手で書く。どちらの手順も
-- メモ帳のコピーをもらう。最後に残せるのは、1冊だけ
bothAtOnce :: StateT Int IO () -> StateT Int IO () -> StateT Int IO ()
bothAtOnce left right = do
  start <- get
  (_afterLeft, afterRight) <- liftIO $
    concurrently (execStateT left start) (execStateT right start)
  put afterRight

-- 🪙 Two clerks add 3 G and 5 G to the day's gold.
-- 受付係2人が、その日の金貨に 3 G と 5 G を足す
lostGold :: IO Int
lostGold = execStateT (bothAtOnce (modify' (+ 3)) (modify' (+ 5))) 0
-- ANCHOR_END: lost

-- ANCHOR: saved
-- 🪙 Do not share the notebook: count in each flow, then add them up.
-- メモ帳を分け合わない：流れごとに数えて、最後に足す
savedGold :: IO Int
savedGold = do
  (a, b) <- concurrently (execStateT (modify' (+ 3)) 0)
                         (execStateT (modify' (+ 5)) 0)
  pure (a + b)
-- ANCHOR_END: saved
-- ANCHOR_END: all
