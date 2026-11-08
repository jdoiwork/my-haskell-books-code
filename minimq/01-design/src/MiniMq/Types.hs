-- ANCHOR: all
-- | The types of minimq: names, messages, bindings, and what can happen.
-- minimq の型：名前、手紙、結びつけ、起こりうること
module MiniMq.Types
  ( QueueName (..)
  , ExchangeName (..)
  , RoutingKey (..)
  , Message (..)
  , Binding (..)
  , Outcome (..)
  , BrokerError (..)
  ) where

-- ANCHOR: imports
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString)
import Data.Text (Text)
-- ANCHOR_END: imports

-- ANCHOR: names
-- 📛 Three kinds of names. All of them are text inside, but the compiler
--    keeps them apart, so a queue name never goes where an exchange
--    name should be.
-- 3種類の名前。中身はどれも文字列だが、コンパイラーが区別するので、
-- 交換所の名前を書くところに、キューの名前は入らない
newtype QueueName = QueueName Text
  deriving newtype (Show, Eq, Ord, IsString)
newtype ExchangeName = ExchangeName Text
  deriving newtype (Show, Eq, Ord, IsString)
newtype RoutingKey = RoutingKey Text
  deriving newtype (Show, Eq, Ord, IsString)
-- ANCHOR_END: names

-- ANCHOR: message
-- ✉️ A message: the address written on it, and what it says.
-- 手紙：表に書いた宛先の札と、中身
data Message = Message
  { routingKey :: RoutingKey
  , body       :: Text
  }
  deriving (Show, Eq)
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
  deriving (Show, Eq)
-- ANCHOR_END: outcome
-- ANCHOR_END: all
