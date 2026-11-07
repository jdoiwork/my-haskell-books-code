-- ANCHOR: all
-- | Answers to the training of "A protocol and a TCP server".
-- 「手順書きと TCP サーバー」のトレーニングの解答
module MqAnswers
  ( publishMany
  , takeLetters
  ) where

import Control.Monad (replicateM)
import qualified Data.Text as T

import MiniMq.Client
import MiniMq.Protocol
import MiniMq.Types
import MiniMq.Wire

-- ANCHOR: publishMany
-- 🚚 Step 2: send every letter first, then read every answer. The
--    answers come back in the same order, so we need not wait for each.
--    (Use a connection that has not sent CONSUME.)
-- ステップ2：先に手紙を全部送り、そのあとで答えを全部読む。
-- 答えは同じ順に返ってくるので、1通ずつ待たなくてよい
-- （CONSUME していないつながりで使う）
publishMany :: Conn -> ExchangeName -> [Message] -> IO [Response]
publishMany conn desk letters = do
  mapM_ (sendRequest conn . Publish desk) letters
  replicateM (length letters) (expect conn)
-- ANCHOR_END: publishMany

-- ANCHOR: takeLetters
-- ✉️ Step 3: consume a queue, but only for `n` letters. Wait for
--    letters only after an OK: if the broker says no, say why and stop.
-- ステップ3：キューから受け取る。ただし n 通だけ。手紙を待つのは、
-- OK のあとだけ。断られたら、理由を言って止まる
takeLetters :: Conn -> QueueName -> Int -> IO [Message]
takeLetters conn q n = request conn (Consume q) >>= \case
  Ok          -> replicateM n (nextLetter conn)
  Refused why -> fail ("cannot consume: " <> T.unpack why)
  other       -> fail ("unexpected answer: " <> show other)
-- ANCHOR_END: takeLetters
-- ANCHOR_END: all
