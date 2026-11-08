-- ANCHOR: all
-- | The pure core of the broker: declare, bind, publish, take.
-- ブローカーの純粋な核：作る、結ぶ、送る、取り出す
module MiniMq.Broker
  ( Broker
  , emptyBroker
  , declareQueue
  , declareExchange
  , bind
  , publish
  , takeNext
  , waiting
  , publishAll
  ) where

-- ANCHOR: imports
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty (nonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..), (|>))

import MiniMq.Types
-- ANCHOR_END: imports

-- ANCHOR: broker
-- 🏤 The whole post office: the sorting desks (exchanges, each with its
--    rules) and the mailboxes (queues, each a line of letters).
-- 伝令所の全体：仕分け台（交換所と、その決まり）と、
-- 郵便受け（キュー。手紙の行列）
data Broker = Broker
  { exchanges :: Map ExchangeName [Binding]
  , queues    :: Map QueueName (Seq Message)
  }
  deriving (Show, Eq)

emptyBroker :: Broker
emptyBroker = Broker Map.empty Map.empty
-- ANCHOR_END: broker

-- ANCHOR: declare
-- 🏗️ Declaring is safe to repeat: what already exists is kept as it is.
-- 作るのは、何度やってもよい。もうあるものは、そのまま残す
declareQueue :: QueueName -> Broker -> Broker
declareQueue name broker =
  broker { queues = Map.insertWith keepOld name Empty (queues broker) }

declareExchange :: ExchangeName -> Broker -> Broker
declareExchange name broker =
  broker { exchanges = Map.insertWith keepOld name [] (exchanges broker) }

keepOld :: new -> old -> old
keepOld _ old = old

-- 🔗 Add a rule to a desk. Both ends must exist.
-- 仕分け台に決まりを足す。両端が、どちらもあること
bind :: ExchangeName -> RoutingKey -> QueueName -> Broker
     -> Either BrokerError Broker
bind name key queue broker = do
  _ <- lookupExchange name broker
  _ <- lookupQueue queue broker
  let rule = Binding key queue
  pure broker { exchanges = Map.adjust (++ [rule]) name (exchanges broker) }
-- ANCHOR_END: declare

-- ANCHOR: lookups
-- 🔍 Find a desk or a mailbox, or say which one is missing.
-- 仕分け台か郵便受けを探す。なければ、どれがないかを言う
lookupExchange :: ExchangeName -> Broker -> Either BrokerError [Binding]
lookupExchange name = maybe (Left (NoSuchExchange name)) Right
                    . Map.lookup name . exchanges

lookupQueue :: QueueName -> Broker -> Either BrokerError (Seq Message)
lookupQueue name = maybe (Left (NoSuchQueue name)) Right
                 . Map.lookup name . queues
-- ANCHOR_END: lookups

-- ANCHOR: publish
-- 📮 Sort one letter: every rule whose key matches sends a copy to its
--    queue (once per queue, even if two rules agree).
-- 手紙を1通仕分ける：札が合う決まりごとに、その郵便受けへ写しを入れる
-- （2つの決まりが同じ郵便受けを指しても、入れるのは1通）
publish :: ExchangeName -> Message -> Broker
        -> Either BrokerError (Outcome, Broker)
publish name message broker = do
  rules <- lookupExchange name broker
  let targets = nubOrd [ target rule
                       | rule <- rules
                       , bindingKey rule == routingKey message ]
  pure $ case nonEmpty targets of
    Nothing -> (Unroutable, broker)
    Just qs -> (Routed qs, foldr (enqueue message) broker qs)

-- 📥 Put a letter at the back of a mailbox.
-- 郵便受けのいちばん後ろに、手紙を入れる
enqueue :: Message -> QueueName -> Broker -> Broker
enqueue message name broker =
  broker { queues = Map.adjust (|> message) name (queues broker) }

-- ANCHOR_END: publish

-- ANCHOR: take
-- 📤 Take the letter at the front of a mailbox, if there is one.
-- 郵便受けの先頭の手紙を取る（あれば）
takeNext :: QueueName -> Broker -> Either BrokerError (Maybe Message, Broker)
takeNext name broker = do
  letters <- lookupQueue name broker
  pure $ case letters of
    Empty          -> (Nothing, broker)
    first :<| rest -> (Just first, setLetters name rest broker)

-- ✏️ Put a new line of letters into a mailbox.
-- 郵便受けの手紙の行列を、新しいものに取りかえる
setLetters :: QueueName -> Seq Message -> Broker -> Broker
setLetters name letters broker =
  broker { queues = Map.insert name letters (queues broker) }
-- ANCHOR_END: take

-- ANCHOR: helpers
-- 👀 How many letters wait in each mailbox.
-- それぞれの郵便受けで待っている手紙の数
waiting :: Broker -> [(QueueName, Int)]
waiting = Map.toList . fmap length . queues

-- 📦 Publish many letters to one desk, keeping every outcome.
--    Each step takes a broker and leaves a new one: `StateT` passes it
--    along, and `traverse` walks the list.
-- 1つの仕分け台へ、手紙をいくつも送る（結果は全部とっておく）。
-- どの一歩も、ブローカーを受け取って新しいのを残す。それを StateT が
-- 次へ渡し、traverse がリストを歩く
publishAll :: ExchangeName -> [Message] -> Broker
           -> Either BrokerError ([Outcome], Broker)
publishAll name messages =
  runStateT (traverse (StateT . publish name) messages)
-- ANCHOR_END: helpers

-- ANCHOR_END: all
