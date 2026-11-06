-- ANCHOR: all
-- | Answers to the training of the "choosing a box" chapter.
-- 「書き換えられる箱の使い分け」の章のトレーニングの解答
module ToolboxAnswers
  ( takeTickets
  , postBounded
  , takeBounded
  , takeWithin
  ) where

import Control.Concurrent.STM
import Data.IORef
import Data.Text (Text)

-- ANCHOR: tickets
-- 🎫 Hand out k numbers in a row, for a party that came together.
-- いっしょに来たパーティーに、続き番号を k 枚まとめて渡す
takeTickets :: IORef Int -> Int -> IO [Int]
takeTickets box k =
  atomicModifyIORef' box (\n -> (n + k, [n .. n + k - 1]))
-- ANCHOR_END: tickets

-- ANCHOR: bounded
-- 📌 Pin a quest; if the board already has `size` quests, wait.
-- 依頼を貼る。もう size 枚貼ってあれば、空くまで待つ
postBounded :: Int -> TVar [Text] -> Text -> STM ()
postBounded size board quest = do
  quests <- readTVar board
  check (length quests < size)
  writeTVar board (quests ++ [quest])

-- 📥 Take the oldest quest; if there is none, wait.
-- いちばん古い依頼を取る。1枚もなければ待つ
takeBounded :: TVar [Text] -> STM Text
takeBounded board = do
  quests <- readTVar board
  case quests of
    []           -> retry
    (q : others) -> do
      writeTVar board others
      pure q
-- ANCHOR_END: bounded

-- ANCHOR: within
-- ⏳ Take a quest from the queue, but wait only `limit` μs.
-- キューから依頼を取る。ただし limit マイクロ秒まで
takeWithin :: Int -> TQueue Text -> IO (Maybe Text)
takeWithin limit queue = do
  late <- registerDelay limit
  atomically $
    (Just <$> readTQueue queue)
      `orElse` (do
        timeIsUp <- readTVar late
        check timeIsUp
        pure Nothing)
-- ANCHOR_END: within
-- ANCHOR_END: all
