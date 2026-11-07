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
--    It keeps this promise only in one process, one letter at a time,
--    for jobs that finished. It breaks if the job throws after its work
--    is done (the job runs again), if two threads ask at once (both see
--    "not yet"), or if the consumer restarts (the set is gone).
-- ステップ2：同じ手紙が2回来ることがあるので、終えた仕事を覚えておき、
-- 1つの仕事は1回だけする（ここでは、本文そのものが仕事の名前）。
-- この約束を守れるのは、1つのプロセスで、1通ずつ順に、最後まで
-- 終わった仕事だけ。仕事が済んだあとで例外が出たら（もう一度動く）、
-- 2つのスレッドが同時に来たら（どちらも「まだ」と読む）、受け手が
-- 起動し直したら（集合ごと消える）、守れない
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
--    "quests" desk again. Only when it got somewhere, answer Ack. When
--    it would go nowhere, stop and leave it (and the rest) in the room.
-- ステップ3：dead-letters の手紙を全部取り出し、quests の仕分け台へ
-- もう一度送る。どこかに届いたときだけ、Ack と返事をする。どこにも
-- 届かないなら、そこでやめて、その手紙（と残り）を部屋に残す
revive :: Broker -> Either BrokerError Broker
revive broker = do
  (who, start) <- subscribe "dead-letters" broker
  let loop b = do
        (got, b1) <- takeNext who 1 "dead-letters" b
        case got of
          Nothing -> pure b1
          Just d  -> do
            (outcome, b2) <- publish "quests" (message d) b1
            case outcome of
              Routed _   -> settle [who] Ack (tag d) b2 >>= loop
              Unroutable -> pure b      -- before it was taken out
  loop start
-- ANCHOR_END: revive
-- ANCHOR_END: all
