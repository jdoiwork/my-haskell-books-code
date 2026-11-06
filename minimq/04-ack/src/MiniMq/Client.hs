-- ANCHOR: all
-- | A plain client: send a request, read what comes back.
-- ふつうのクライアント：頼みごとを送り、返ってきたものを読む
module MiniMq.Client
  ( withClient
  , request
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

-- 📮 Send one request and wait for its answer.
-- 頼みごとを1つ送り、その答えを待つ
request :: Conn -> Request -> IO Response
request conn r = send conn r >> expect conn

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
