-- ANCHOR: all
-- | A plain client: send a request, read what comes back. Keep two
--   connections: one to ask (declare, bind, publish), and one to
--   receive. After CONSUME, letters may come at any time, so on that
--   connection, only wait for letters and answer them (ACK and friends
--   get no reply when all is well).
-- ふつうのクライアント：頼みごとを送り、返ってきたものを読む。
-- つながりは2本に分ける：頼む（作る・結ぶ・送る）ための1本と、
-- 受け取るための1本。CONSUME したあとは、いつ手紙が来るかわからない
-- ので、そのつながりでは、手紙を待って、返事をすることだけをする
-- （ACK などは、まちがいがなければ、答えが返らない）
module MiniMq.Client
  ( withClient
  , request
  , sendRequest
  , expect
  , nextDelivery
  , answerWith
  ) where

import Control.Exception (bracket)
import Network.Socket

import MiniMq.Protocol
import MiniMq.Types
import MiniMq.Wire

-- 🔌 Connect to a broker on this machine.
-- この機械のブローカーにつなぐ
withClient :: PortNumber -> (Conn -> IO a) -> IO a
withClient port = bracket open (close . connSocket)
  where
    open = do
      sock <- socket AF_INET Stream defaultProtocol
      connect sock (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
      newConn sock

-- 📮 Send one request and wait for its answer: the next thing that
--    comes. (On a connection that sent CONSUME, that might be a letter.)
-- 頼みごとを1つ送り、その答えを待つ。答えとは、次に来るもののこと
-- （CONSUME したつながりでは、それが手紙かもしれない）
request :: Conn -> Request -> IO Response
request conn r = sendRequest conn r >> expect conn

-- 🚧 Send one request, after checking the rule of the wire. A name
--    with a space or a newline would be read as a different request.
-- 手順書きの決まりを確かめてから、頼みごとを1つ送る。空白や改行の入った
-- 名前は、別の頼みごととして読まれてしまう
sendRequest :: Conn -> Request -> IO ()
sendRequest conn r = case brokenNames r of
  []  -> send conn r
  bad -> fail ("a name must be one word: " <> show bad)

-- 📨 The next thing the broker says.
-- ブローカーが次に言うこと
expect :: Conn -> IO Response
expect conn = receive conn >>= \case
  Got response -> pure response
  Closed       -> fail "the broker hung up"
  Garbled bad  -> fail ("cannot read the broker: " <> show bad)

-- ✉️ Wait for the next letter (after CONSUME).
-- 次の手紙を待つ（CONSUME のあとで）
nextDelivery :: Conn -> IO Delivery
nextDelivery conn = expect conn >>= \case
  Deliver delivery -> pure delivery
  other            -> fail ("expected a letter, got " <> show other)

-- ✅ Answer a letter: ACK, REQUEUE or REJECT. Nothing comes back.
-- 手紙に返事をする：ACK、REQUEUE、REJECT。何も返ってこない
answerWith :: Settlement -> Conn -> Delivery -> IO ()
answerWith how conn delivery = send conn (Settle how (tag delivery))
-- ANCHOR_END: all
