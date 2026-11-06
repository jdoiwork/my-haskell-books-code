-- ANCHOR: all
-- | Answers to the training of "A protocol and a TCP server".
-- 「手順書きと TCP サーバー」のトレーニングの解答
module MqAnswers
  ( publishMany
  , takeLetters
  ) where

import Control.Monad (replicateM)

import MiniMq.Client
import MiniMq.Protocol
import MiniMq.Types
import MiniMq.Wire

-- ANCHOR: publishMany
-- 🚚 Step 2: send every letter first, then read every answer. The
--    answers come back in the same order, so we need not wait for each.
-- ステップ2：先に手紙を全部送り、そのあとで答えを全部読む。
-- 答えは同じ順に返ってくるので、1通ずつ待たなくてよい
publishMany :: Conn -> ExchangeName -> [Message] -> IO [Response]
publishMany conn desk letters = do
  mapM_ (send conn . Publish desk) letters
  replicateM (length letters) (expect conn)
-- ANCHOR_END: publishMany

-- ANCHOR: takeLetters
-- ✉️ Step 3: consume a queue, but only for `n` letters.
-- ステップ3：キューから受け取る。ただし n 通だけ
takeLetters :: Conn -> QueueName -> Int -> IO [Message]
takeLetters conn q n = do
  _ <- request conn (Consume q)
  replicateM n (nextLetter conn)
-- ANCHOR_END: takeLetters
-- ANCHOR_END: all
