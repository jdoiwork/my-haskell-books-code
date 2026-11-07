-- ANCHOR: header
{-# LANGUAGE DeriveAnyClass #-}
-- ANCHOR: all
-- | The types of minimq: names, messages, bindings, and what can happen.
-- minimq の型：名前、手紙、結びつけ、起こりうること
module MiniMq.Types
  ( QueueName (..)
  , ExchangeName (..)
  , RoutingKey (..)
  , Message (..)
  , Binding (..)
  , ExchangeKind (..)
  , Exchange (..)
  , Outcome (..)
  , BrokerError (..)
  , QueueSettings (..)
  , Durability (..)
  , plainQueue
  , Tag (..)
  , ConsumerId (..)
  , Delivery (..)
  , Settlement (..)
  , LetterId (..)
  , Entry (..)
  ) where

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString)
import Data.Text (Text)
-- ANCHOR_END: header

-- ANCHOR: names
-- 📛 Three kinds of names. All of them are text inside, but the compiler
--    keeps them apart, so a queue name never goes where an exchange
--    name should be.
-- 3種類の名前。中身はどれも文字列だが、コンパイラーが区別するので、
-- 交換所の名前を書くところに、キューの名前は入らない
newtype QueueName = QueueName Text
  deriving newtype (Show, Eq, Ord, IsString, ToJSON, FromJSON)
newtype ExchangeName = ExchangeName Text
  deriving newtype (Show, Eq, Ord, IsString, ToJSON, FromJSON)
newtype RoutingKey = RoutingKey Text
  deriving newtype (Show, Eq, Ord, IsString, ToJSON, FromJSON)
-- ANCHOR_END: names

-- ANCHOR: message
-- ✉️ A message: the address written on it, and what it says.
-- 手紙：表に書いた宛先の札と、中身
data Message = Message
  { routingKey :: RoutingKey
  , body       :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
-- ANCHOR_END: message

-- ANCHOR: binding
-- 🔗 A rule of the sorting desk: "letters with this key go to that queue".
-- 仕分けの決まり：「この札の手紙は、あのキューへ」
data Binding = Binding
  { bindingKey :: RoutingKey
  , target     :: QueueName
  }
  deriving (Show, Eq)
-- ANCHOR_END: binding

-- ANCHOR: kind
-- 🔀 How a sorting desk hands out letters. Three ways, and no other:
--    the compiler checks that every function says what to do with each.
-- 仕分け台の配り方。この3つだけ。どの関数も、それぞれをどう扱うかを
-- 書いているか、コンパイラーが確かめる
data ExchangeKind
  = Fanout   -- ^ to every bound queue / 結んだキューの全員に
  | Direct   -- ^ when the key is exactly the same / 札がぴったり同じなら
  | Topic    -- ^ when the key fits the pattern / 札が模様に合えば
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 🗂️ A sorting desk: how it hands out, and its rules.
-- 仕分け台：配り方と、決まりの一覧
data Exchange = Exchange
  { kind  :: ExchangeKind
  , rules :: [Binding]
  }
  deriving (Show, Eq)
-- ANCHOR_END: kind

-- ANCHOR: outcome
-- 📮 What became of a published message. `Routed` always names at least
--    one queue; "nowhere" is a case of its own.
-- 送った手紙がどうなったか。Routed には、いつも1つ以上のキューがある。
-- 「どこにも届かなかった」は、別の場合にする
data Outcome
  = Routed (NonEmpty QueueName)
  | Unroutable
  deriving (Show, Eq)

-- 🚫 Asking for something that does not exist.
-- ないものを頼んだ
data BrokerError
  = NoSuchQueue    QueueName
  | NoSuchExchange ExchangeName
  | UnknownTag     Tag          -- ^ new: no letter waits for this answer
  deriving (Show, Eq)

-- (The server throws it inside STM, so it is an exception too.)
-- （サーバーが STM の中で投げるので、例外にもしておく）
instance Exception BrokerError
-- ANCHOR_END: outcome

-- ANCHOR: delivery
-- 🎫 The number on a handed-out letter. The consumer answers with it.
-- 渡した手紙の番号札。受け手は、この番号で返事をする
newtype Tag = Tag Int
  deriving newtype (Show, Eq, Ord)

-- 🙋 Who holds the letters: one number per CONSUME.
-- 手紙を持っている人：CONSUME ごとに1つの番号
newtype ConsumerId = ConsumerId Int
  deriving newtype (Show, Eq, Ord)

-- 📨 A letter as a consumer gets it: its tag, and whether someone had it
--    before (and did not finish).
-- 受け手に渡る手紙：番号札と、前にほかの誰かが持っていたか
-- （そして、終わらせなかったか）
data Delivery = Delivery
  { tag         :: Tag
  , message     :: Message
  , redelivered :: Bool
  }
  deriving (Show, Eq)

-- ✅ The three answers a consumer can give.
-- 受け手ができる、3つの返事
data Settlement
  = Ack       -- ^ done; forget it / 終わった。忘れてよい
  | Requeue   -- ^ not now; give it to someone / 今は無理。誰かに渡して
  | Reject    -- ^ never; it is broken / ずっと無理。壊れている
  deriving (Show, Eq, Enum, Bounded)
-- ANCHOR_END: delivery

-- ANCHOR: settings
-- ⚙️ How a queue treats letters that fail: where they go when given up
--    (a dead-letter exchange), and how many tries they get.
-- うまくいかなかった手紙の扱い方：あきらめたらどこへ送るか
-- （配れなかった手紙の交換所）と、何回まで試すか
data QueueSettings = QueueSettings
  { deadLetterTo  :: Maybe ExchangeName
  , deliveryLimit :: Maybe Int
  , durability    :: Durability     -- ^ new / 新しく
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 💾 Should the queue (and its letters) live through a crash?
-- キュー（とその手紙）は、落ちても残るべきか
data Durability = Durable | Transient
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

plainQueue :: QueueSettings
plainQueue = QueueSettings Nothing Nothing Durable
-- ANCHOR_END: settings

-- ANCHOR: entry
-- 🪪 Every copy of a letter in a queue gets its own number, so that the
--    journal can say "this one is finished".
-- キューに入った手紙の写しには、1通ずつ番号を付ける。日誌に
-- 「この手紙は終わった」と書けるように
newtype LetterId = LetterId Int
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

-- 📜 One line of the journal: a fact that must outlive a crash.
--    Handing a letter out is not written down: after a crash, every
--    letter that was out comes back anyway.
-- 日誌の1行：落ちても残すべき事実。手紙を渡したことは書かない。
-- 落ちたら、渡していた手紙は、どのみち全部戻ってくるから
data Entry
  = MadeQueue    QueueName QueueSettings
  | MadeExchange ExchangeName ExchangeKind
  | Bound        ExchangeName RoutingKey QueueName
  | Stored       QueueName LetterId Message   -- ^ came in / 届いた
  | Finished     LetterId                     -- ^ acked or given up
                                              --   片付いた
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
-- ANCHOR_END: entry
-- ANCHOR_END: all
