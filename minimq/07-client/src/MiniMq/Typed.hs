-- ANCHOR: all
-- | A client that knows what is in each letter. Desks and mailboxes carry
--   the type of their letters; a handler only ever sees a value, never
--   a tag, so it cannot forget to answer, or answer twice.
-- 手紙の中身を知っているクライアント。仕分け台と郵便受けは、手紙の型を
-- 持っている。受け手の関数に渡るのは値だけで、番号札は渡らない。
-- だから、返事を忘れることも、2回返事をすることもできない
-- ANCHOR: header
module MiniMq.Typed
  ( Codec (..)
  , json
  , Desk (..)
  , Mailbox (..)
  , MqError (..)
  , declareDesk
  , declareMailbox
  , bindTo
  , publishTo
  , consume
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)

import MiniMq.Client
import MiniMq.Protocol
import MiniMq.Types
import MiniMq.Wire
-- ANCHOR_END: header

-- ANCHOR: codec
-- 🧪 How an `a` becomes the text of a letter, and back.
-- a を手紙の本文にする方法と、その逆
data Codec a = Codec
  { toBody   :: a -> Text
  , fromBody :: Text -> Either Text a
  }

-- 📦 Any type aeson knows can travel as JSON.
-- aeson が知っている型なら、どれも JSON で運べる
json :: (ToJSON a, FromJSON a) => Codec a
json = Codec
  { toBody   = decodeUtf8Lenient . LBS.toStrict . encode
  , fromBody = either (Left . T.pack) Right . eitherDecodeStrict . encodeUtf8
  }
-- ANCHOR_END: codec

-- ANCHOR: desk
-- 🗂️ A desk that only takes `a`s, and a mailbox that only holds `a`s.
--    The `a` is in the type, so the compiler can see when two disagree.
-- a だけを受け付ける仕分け台と、a だけが入る郵便受け。
-- a が型に入っているので、食いちがえばコンパイラーにわかる
data Desk a = Desk
  { deskName  :: ExchangeName
  , deskKind  :: ExchangeKind
  , deskCodec :: Codec a
  }

data Mailbox a = Mailbox
  { boxName     :: QueueName
  , boxSettings :: QueueSettings
  , boxCodec    :: Codec a
  }
-- ANCHOR_END: desk

-- ANCHOR: error
-- 🚫 What the broker refused, or an answer we did not expect.
-- ブローカーに断られた、または、思いがけない答えが来た
data MqError = Refusal Text | Unexpected Response
  deriving Show

instance Exception MqError
-- ANCHOR_END: error

-- ANCHOR: declare
-- 🏗️ Make them, and tie a desk to a mailbox. Both must carry the same
--    `a`: a desk of quests cannot feed a mailbox of herbs.
-- 作って、仕分け台と郵便受けを結ぶ。両方とも同じ a でなければならない。
-- 依頼の仕分け台から、薬草の郵便受けへは、つなげない
declareDesk :: Conn -> Desk a -> IO ()
declareDesk conn desk =
  expectOk =<< request conn (DeclareExchange (deskName desk) (deskKind desk))

declareMailbox :: Conn -> Mailbox a -> IO ()
declareMailbox conn box =
  expectOk =<< request conn (DeclareQueue (boxName box) (boxSettings box))

bindTo :: Conn -> Desk a -> RoutingKey -> Mailbox a -> IO ()
bindTo conn desk key box =
  expectOk =<< request conn (Bind (deskName desk) key (boxName box))

expectOk :: Response -> IO ()
expectOk Ok            = pure ()
expectOk (Refused why) = throwIO (Refusal why)
expectOk other         = throwIO (Unexpected other)
-- ANCHOR_END: declare

-- ANCHOR: publish
-- 📮 Send an `a`; answer how many mailboxes got it.
-- a を1つ送る。いくつの郵便受けに届いたかを答える
publishTo :: Conn -> Desk a -> RoutingKey -> a -> IO Int
publishTo conn desk key value = do
  let letter = Message key (toBody (deskCodec desk) value)
  request conn (Publish (deskName desk) letter) >>= \case
    RoutedTo n    -> pure n
    NoRoute       -> pure 0
    Refused why   -> throwIO (Refusal why)
    other         -> throwIO (Unexpected other)
-- ANCHOR_END: publish

-- ANCHOR: consume
-- 📬 Give every letter to `handle`. How `handle` ends is the answer:
--    it returns, and we say ACK; it throws, and we say nothing at all
--    and fall over with it. The connection closes, and the broker puts
--    the letter back by itself. (Let it crash: a supervisor starts us
--    again.) A letter we cannot even read is rejected at once.
-- 手紙を1通ずつ handle に渡す。handle の終わり方が、そのまま返事になる：
-- 戻ってきたら ACK。例外を投げたら、何も言わずに、いっしょに倒れる。
-- つながりが閉じ、ブローカーが自分で手紙を戻す（倒れるなら倒れれば
-- いい。監督役が始め直す）。読むことさえできない手紙は、すぐに REJECT
consume :: Conn -> Mailbox a -> (a -> IO ()) -> IO ()
consume conn box handle = do
  expectOk =<< request conn (Consume (boxName box) 1)
  forever $ do
    delivery <- nextDelivery conn
    let answer how = answerWith how conn delivery
    case fromBody (boxCodec box) (body (message delivery)) of
      Left _      -> answer Reject
      Right value -> handle value >> answer Ack
-- ANCHOR_END: consume


-- ANCHOR_END: all
