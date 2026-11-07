-- ANCHOR: all
-- | A post office with a dead-letter room, to try in GHCi.
-- GHCi で試すための、「配れなかった手紙の部屋」のある伝令所
module MiniMq.Examples
  ( surely
  , guildPost
  , cursedPost
  , answerOnce
  , answerEach
  ) where

import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Function ((&))

import MiniMq.Broker
import MiniMq.Types

-- ANCHOR: post
-- 🏤 Jobs get three tries. A letter that runs out of tries goes to the
--    "undeliverable" desk, which puts it in the "dead-letters" room.
-- 仕事の手紙は3回まで。使い切った手紙は「undeliverable」の仕分け台へ
-- 行き、そこから「dead-letters」の部屋に入る
guildPost :: Either BrokerError Broker
guildPost =
  emptyBroker
    & declareExchange "quests"        Direct
    & declareExchange "undeliverable" Fanout
    & declareQueue "jobs" plainQueue
        { deadLetterTo  = Just "undeliverable"
        , deliveryLimit = Just 3
        }
    & declareQueue "dead-letters" plainQueue
    & bind "quests" "job" "jobs"
    >>= bind "undeliverable" "" "dead-letters"

-- 🧟 The same, with one letter in "jobs" that no one can finish.
-- 同じ伝令所に、誰にも終わらせられない手紙が1通
cursedPost :: Either BrokerError Broker
cursedPost = do
  post <- guildPost
  snd <$> publish "quests" (Message "job" "Open the cursed box") post
-- ANCHOR_END: post

-- ANCHOR: answer
-- 🙋 A new consumer takes the next letter from "jobs" and answers `how`.
-- 新しい受け手が jobs から次の手紙を取り、how と返事をする
answerOnce :: Settlement -> Broker
           -> Either BrokerError (Maybe Delivery, Broker)
answerOnce how broker = do
  (who,  broker1) <- subscribe "jobs" broker
  (got,  broker2) <- takeNext who 1 "jobs" broker1
  case got of
    Nothing       -> pure (Nothing, broker2)
    Just delivery -> do
      broker3 <- settle [who] how (tag delivery) broker2
      pure (got, broker3)

-- 🔁 One consumer after another, each answering as told.
-- 受け手が1人ずつ、言われたとおりに返事をしていく
answerEach :: [Settlement] -> Broker
           -> Either BrokerError ([Maybe Delivery], Broker)
answerEach hows = runStateT (traverse (StateT . answerOnce) hows)
-- ANCHOR_END: answer
-- 🙆 For GHCi: take the result out of steps we know cannot fail.
-- GHCi 用：失敗しないとわかっている手順から、結果を取り出す
surely :: Show e => Either e a -> a
surely = either (error . show) id
-- ANCHOR_END: all
