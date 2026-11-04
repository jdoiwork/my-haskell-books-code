-- ANCHOR: all
-- 🏋️ Answers for the training.
-- トレーニングの解答
module ConcurrencyAnswers (fastestScout, tryTransfer) where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM

import Scouts (scout)

-- ANCHOR: fastest
-- 🏃 Send two scouts; take the report of whoever comes back first.
-- 2人の偵察を送り、先に戻った人の報告を受け取る
fastestScout :: (String, Int) -> (String, Int) -> IO String
fastestScout a b = do
  winner <- race (scout a) (scout b)
  pure (either id id winner)
-- ANCHOR_END: fastest

-- ANCHOR: try
-- 💸 Move gold only if there is enough; say whether it moved.
-- 足りるときだけ金貨を移し、移せたかどうかを返す
tryTransfer :: TVar Int -> TVar Int -> Int -> STM Bool
tryTransfer from to amount = do
  balance <- readTVar from
  if balance < amount
    then pure False
    else do
      modifyTVar' from (subtract amount)
      modifyTVar' to   (+ amount)
      pure True
-- ANCHOR_END: try
-- ANCHOR_END: all
