-- ANCHOR: all
-- | Answers to the training of "Design: messages, queues and routing".
-- 「設計：メッセージ・キュー・配り方を型で描く」のトレーニングの解答
module MqAnswers
  ( drain
  , purge
  ) where

import MiniMq.Broker
import MiniMq.Types

-- ANCHOR: drain
-- 📤 Step 2: take every letter from a mailbox, in order.
-- ステップ2：郵便受けの手紙を、順に全部取る
drain :: QueueName -> Broker -> Either BrokerError ([Message], Broker)
drain name broker = do
  (next, broker') <- takeNext name broker
  case next of
    Nothing     -> pure ([], broker')
    Just letter -> do
      (rest, broker'') <- drain name broker'
      pure (letter : rest, broker'')
-- ANCHOR_END: drain

-- ANCHOR: purge
-- 🗑️ Step 3: throw away every letter, and say how many there were.
-- ステップ3：手紙を全部捨てて、何通あったかを言う
purge :: QueueName -> Broker -> Either BrokerError (Int, Broker)
purge name broker = do
  (letters, broker') <- drain name broker
  pure (length letters, broker')
-- ANCHOR_END: purge
-- ANCHOR_END: all
