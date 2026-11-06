-- ANCHOR: all
-- | Waiting with STM: until the party gathers, until there is a free
--   table, or until time runs out.
-- STM で待つ：パーティーがそろうまで、空いた机ができるまで、時間切れまで
module Gather
  ( arrive
  , waitForParty
  , seatParty
  , waitPartyFor
  , gatherDemo
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM_)
import Data.Text (Text)

-- ANCHOR: wait
-- 🙋 Someone arrives at the guild hall.
-- だれかが、ギルドの広間に着く
arrive :: TVar [Text] -> Text -> STM ()
arrive party name = modifyTVar' party (++ [name])

-- 🧑‍🤝‍🧑 Wait until the party has enough members.
-- パーティーの人数がそろうまで待つ
waitForParty :: Int -> TVar [Text] -> STM [Text]
waitForParty size party = do
  members <- readTVar party
  check (length members >= size)
  pure members
-- ANCHOR_END: wait

-- ANCHOR: seat
-- 🪑 Wait until the party is ready AND a table is free; then take it.
-- パーティーがそろい、しかも机が空くまで待って、机を1つ取る
seatParty :: Int -> TVar [Text] -> TVar Int -> STM [Text]
seatParty size party tables = do
  members <- waitForParty size party
  free    <- readTVar tables
  check (free > 0)
  writeTVar tables (free - 1)
  pure members
-- ANCHOR_END: seat

-- ANCHOR: timeout
-- ⏳ Wait for the party, but only for so long (μs).
-- パーティーを待つ。ただし、決めた時間（マイクロ秒）まで
waitPartyFor :: Int -> Int -> TVar [Text] -> IO (Maybe [Text])
waitPartyFor limit size party = do
  late <- registerDelay limit
  atomically $
    (Just <$> waitForParty size party)
      `orElse` (do
        timeIsUp <- readTVar late
        check timeIsUp
        pure Nothing)
-- ANCHOR_END: timeout

-- ANCHOR: demo
-- 🧪 The members arrive 0.1 seconds apart. Wait for three of them.
-- 0.1秒おきに、メンバーが着く。3人そろうのを待つ
gatherDemo :: [Text] -> Int -> IO (Maybe [Text])
gatherDemo names limit = do
  party <- newTVarIO []
  _ <- forkIO $ forM_ names $ \name -> do
    threadDelay 100000
    atomically (arrive party name)
  waitPartyFor limit 3 party
-- ANCHOR_END: demo
-- ANCHOR_END: all
