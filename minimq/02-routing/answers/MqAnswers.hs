-- ANCHOR: all
-- | Answers to the training of "Routing rules".
-- 「配り方の規則」のトレーニングの解答
module MqAnswers
  ( dryRun
  , lostKeys
  ) where

import Control.Monad (filterM)

import MiniMq.Broker
import MiniMq.Types

-- ANCHOR: dryRun
-- 🧪 Step 2: where would this key go? `publish` is pure, so we just
--    throw the new broker away: nothing was put anywhere.
-- ステップ2：この札なら、どこへ行く？ publish は純粋なので、
-- 新しいブローカーを捨てるだけ。どこにも何も入らない
dryRun :: ExchangeName -> RoutingKey -> Broker -> Either BrokerError Outcome
dryRun name key broker = fst <$> publish name (Message key "") broker
-- ANCHOR_END: dryRun

-- ANCHOR: lostKeys
-- 🕳️ Step 3: which of these keys would reach no queue at all?
-- ステップ3：この札のうち、どのキューにも届かないのはどれ？
lostKeys :: ExchangeName -> [RoutingKey] -> Broker
         -> Either BrokerError [RoutingKey]
lostKeys name keys broker = filterM isLost keys
  where
    isLost key = (== Unroutable) <$> dryRun name key broker
-- ANCHOR_END: lostKeys
-- ANCHOR_END: all
