-- ANCHOR: all
-- | Answers to the training of "Acknowledgements and redelivery".
-- 「受け取りの確認と配り直し」のトレーニングの解答
module MqAnswers
  ( onlyOnce
  , revive
  ) where

import Data.IORef
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)

import MiniMq.Broker
import MiniMq.Types

-- ANCHOR: onlyOnce
-- 🔂 Step 2: a letter may come twice, so remember what was done and do
--    each job only once. (Here the body itself is the job's name.)
-- ステップ2：同じ手紙が2回来ることがあるので、終えた仕事を覚えておき、
-- 1つの仕事は1回だけする（ここでは、本文そのものが仕事の名前）
onlyOnce :: IORef (Set Text) -> (Text -> IO ()) -> Delivery -> IO ()
onlyOnce doneRef job delivery = do
  let name = body (message delivery)
  seen <- Set.member name <$> readIORef doneRef
  if seen
    then pure ()
    else do
      job name
      modifyIORef doneRef (Set.insert name)
-- ANCHOR_END: onlyOnce

-- ANCHOR: revive
-- 🧪 Step 3: take every letter out of "dead-letters" and send it to the
--    "quests" desk again, answering Ack for each one.
-- ステップ3：dead-letters の手紙を全部取り出し、quests の仕分け台へ
-- もう一度送る。1通ごとに Ack と返事をする
revive :: Broker -> Either BrokerError Broker
revive broker = do
  (who, start) <- subscribe "dead-letters" broker
  let loop b = do
        (got, b1) <- takeNext who 1 "dead-letters" b
        case got of
          Nothing -> pure b1
          Just d  -> do
            b2 <- settle Ack (tag d) b1
            (_, b3) <- publish "quests" (message d) b2
            loop b3
  loop start
-- ANCHOR_END: revive
-- ANCHOR_END: all
