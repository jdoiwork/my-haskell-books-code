-- ANCHOR: all
-- ANCHOR: header
-- | The pure core, now with answers: a handed-out letter is kept until
--   its consumer says it is done.
-- 純粋な核。返事つきになった：渡した手紙は、受け手が「終わった」と
-- 言うまで、とっておく
module MiniMq.Broker
  ( Broker
  , emptyBroker
  , declareQueue
  , declareExchange
  , bind
  , publish
  , subscribe
  , takeNext
  , settle
  , abandon
  , lookupQueue
  , waiting
  , unsettled
  , publishAll
  ) where

import Control.Monad.Trans.State.Strict (StateT (..))
import Data.List.NonEmpty (nonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..), (<|), (|>))

import MiniMq.Routing
import MiniMq.Types
-- ANCHOR_END: header

-- ANCHOR: broker
-- 🏤 The post office. New: each queue has its settings, and there is
--    a book of letters that were handed out but not yet answered.
-- 伝令所。新しく：キューごとの設定と、「渡したけれど、まだ返事の
-- ない手紙」の帳面
data Broker = Broker
  { exchanges :: !(Map ExchangeName Exchange)
  , queues    :: !(Map QueueName Queue)
  , pending   :: !(Map Tag Pending)
  , nextTag   :: !Int
  , nextId    :: !Int
  }
  deriving (Show, Eq)

-- 📪 A queue: its settings, and the letters ready to hand out.
-- キュー：設定と、渡せる手紙の行列
data Queue = Queue
  { settings :: QueueSettings
  , ready    :: Seq Envelope
  }
  deriving (Show, Eq)

-- ✉️ A letter in its envelope, which counts how often it went out.
-- 封筒に入った手紙。何回渡したかを数える
data Envelope = Envelope
  { letter  :: Message
  , handOut :: Int
  }
  deriving (Show, Eq)

-- 📒 One line of the book: who holds the letter, and from which queue.
-- 帳面の1行：誰が持っているか、どのキューから来たか
data Pending = Pending
  { holder   :: ConsumerId
  , fromQ    :: QueueName
  , envelope :: Envelope
  }
  deriving (Show, Eq)
-- ANCHOR_END: broker

-- ANCHOR: core
-- 🆕 A post office with nothing in it yet.
-- まだ何もない伝令所
emptyBroker :: Broker
emptyBroker = Broker Map.empty Map.empty Map.empty 1 1

-- 🏗️ Declaring is safe to repeat: what already exists is kept as it is.
-- 作るのは、何度やってもよい。もうあるものは、そのまま残す
declareQueue :: QueueName -> QueueSettings -> Broker -> Broker
declareQueue name how broker =
  broker { queues = Map.insertWith keepOld name (Queue how Empty)
                                   (queues broker) }

declareExchange :: ExchangeName -> ExchangeKind -> Broker -> Broker
declareExchange name how broker =
  broker { exchanges = Map.insertWith keepOld name (Exchange how [])
                                      (exchanges broker) }

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
      addRule exchange = exchange { rules = rules exchange ++ [rule] }
  pure broker { exchanges = Map.adjust addRule name (exchanges broker) }

-- 🔍 Find a desk or a mailbox, or say which one is missing.
-- 仕分け台か郵便受けを探す。なければ、どれがないかを言う
lookupExchange :: ExchangeName -> Broker -> Either BrokerError Exchange
lookupExchange name = maybe (Left (NoSuchExchange name)) Right
                    . Map.lookup name . exchanges

findQueue :: QueueName -> Broker -> Either BrokerError Queue
findQueue name = maybe (Left (NoSuchQueue name)) Right
               . Map.lookup name . queues

lookupQueue :: QueueName -> Broker -> Either BrokerError QueueSettings
lookupQueue name broker = settings <$> findQueue name broker

-- 📮 Sort one letter: a fresh envelope for each queue it goes to.
-- 手紙を1通仕分ける：届くキューごとに、新しい封筒に入れて
publish :: ExchangeName -> Message -> Broker
        -> Either BrokerError (Outcome, Broker)
publish name message broker = do
  exchange <- lookupExchange name broker
  pure $ case nonEmpty (route exchange (routingKey message)) of
    Nothing -> (Unroutable, broker)
    Just qs -> (Routed qs, foldr (atBack (Envelope message 0)) broker qs)
-- ANCHOR_END: core

-- ANCHOR: take
-- 🙋 A new consumer for a queue gets its own number.
-- キューに新しい受け手が来たら、番号を1つ渡す
subscribe :: QueueName -> Broker -> Either BrokerError (ConsumerId, Broker)
subscribe name broker = do
  _ <- lookupQueue name broker
  pure (ConsumerId (nextId broker), broker { nextId = nextId broker + 1 })

-- 📤 Hand the next letter to a consumer, if it may hold one more
--    (`limit` is its prefetch). The letter does not vanish: it moves
--    into the book, under a new tag.
-- 受け手が、もう1通持ってよいなら（limit は先に受け取れる数）、
-- 次の手紙を渡す。手紙は消えない：新しい番号札を付けて、帳面へ移る
takeNext :: ConsumerId -> Int -> QueueName -> Broker
         -> Either BrokerError (Maybe Delivery, Broker)
takeNext who limit name broker = do
  queue <- findQueue name broker
  pure $ case ready queue of
    next :<| rest | holding who broker < limit ->
      let number  = Tag (nextTag broker)
          held    = Pending who name next { handOut = handOut next + 1 }
          broker' = overReady name (const rest) broker
      in  ( Just (Delivery number (letter next) (handOut next > 0))
          , broker' { pending = Map.insert number held (pending broker')
                    , nextTag = nextTag broker + 1 } )
    _ -> (Nothing, broker)

-- 🔢 How many unanswered letters a consumer holds.
-- 受け手が持っている、返事のまだない手紙の数
holding :: ConsumerId -> Broker -> Int
holding who = length . filter ((== who) . holder) . Map.elems . pending
-- ANCHOR_END: take

-- ANCHOR: settle
-- ✅ A consumer answers. `mine` are the consumers on the line that
--    answers: only a letter one of them holds can be answered. Then the
--    letter leaves the book, and: Ack forgets it; Requeue puts it back
--    at the front, unless it has used up its tries; Reject gives it up.
-- 受け手が返事をする。mine は、返事をしたつながりの受け手たち。その
-- 誰かが持っている手紙にだけ、返事ができる。手紙は帳面から出て、
-- それから：Ack なら忘れる。Requeue なら先頭に戻す（試せる回数を
-- 使い切っていなければ）。Reject なら、すぐにあきらめる
settle :: [ConsumerId] -> Settlement -> Tag -> Broker
       -> Either BrokerError Broker
settle mine how number broker = do
  held <- case Map.lookup number (pending broker) of
    Just p | holder p `elem` mine -> Right p
    _                             -> Left (UnknownTag number)
  let broker' = broker { pending = Map.delete number (pending broker) }
  pure $ case how of
    Ack                          -> broker'
    Reject                       -> giveUp held broker'
    Requeue | usedUp held broker -> giveUp held broker'
            | otherwise          -> atFront held broker'

-- 🚪 A consumer went away: every letter it held goes back, as if it
--    had said Requeue (so a letter that keeps knocking consumers over
--    still runs out of tries).
-- 受け手がいなくなった：持っていた手紙は全部、Requeue と言われたように
-- 戻る（受け手を何度も倒す手紙も、いずれ回数を使い切る）
abandon :: ConsumerId -> Broker -> Broker
abandon who broker = foldl' requeue broker (reverse (Map.keys theirs))
  where
    theirs = Map.filter ((== who) . holder) (pending broker)
    requeue b number = either (const b) id (settle [who] Requeue number b)
-- ANCHOR_END: settle

-- ANCHOR: deadletter
-- 🔁 Has this letter had all the tries its queue allows?
-- この手紙は、キューが許す回数を使い切ったか
usedUp :: Pending -> Broker -> Bool
usedUp held broker =
  case lookupQueue (fromQ held) broker of
    Right QueueSettings { deliveryLimit = Just most } ->
      handOut (envelope held) >= most
    _ -> False

-- 🪦 Give a letter up: send it to the queue's dead-letter exchange, as
--    a fresh letter. It is dropped, just as in RabbitMQ, when the queue
--    names no such exchange, when that exchange does not exist, or when
--    the exchange sends it nowhere (no binding fits).
-- 手紙をあきらめる：キューの「配れなかった手紙」の交換所へ、新しい手紙と
-- して送る。次の3つのときは、RabbitMQ と同じく、捨てる：キューに
-- 行き先の設定がない、その交換所がない、結び付けがなく、どこにも届かない
giveUp :: Pending -> Broker -> Broker
giveUp held broker =
  case lookupQueue (fromQ held) broker of
    Right QueueSettings { deadLetterTo = Just desk } ->
      either (const broker) snd (publish desk (letter (envelope held)) broker)
    _ -> broker
-- ANCHOR_END: deadletter

-- ANCHOR: envelopes
-- 📥 Put an envelope at the back of a queue, or back at its front.
-- 封筒をキューの後ろに入れる。または、先頭に戻す
atBack :: Envelope -> QueueName -> Broker -> Broker
atBack env name = overReady name (|> env)

atFront :: Pending -> Broker -> Broker
atFront held = overReady (fromQ held) (envelope held <|)

-- ✏️ Change the line of letters ready in a queue.
-- キューの、渡せる手紙の行列を書きかえる
overReady :: QueueName -> (Seq Envelope -> Seq Envelope) -> Broker -> Broker
overReady name f broker =
  broker { queues = Map.adjust (\q -> q { ready = f (ready q) }) name
                               (queues broker) }

-- 👀 Letters waiting in each queue, and letters out without an answer.
-- それぞれのキューで待っている手紙の数と、返事のない手紙の数
waiting :: Broker -> [(QueueName, Int)]
waiting = Map.toList . fmap (length . ready) . queues

unsettled :: Broker -> Int
unsettled = Map.size . pending

-- 📦 Publish many letters to one desk, keeping every outcome.
-- 1つの仕分け台へ、手紙をいくつも送る（結果は全部とっておく）
publishAll :: ExchangeName -> [Message] -> Broker
           -> Either BrokerError ([Outcome], Broker)
publishAll name messages =
  runStateT (traverse (StateT . publish name) messages)
-- ANCHOR_END: envelopes
-- ANCHOR_END: all
