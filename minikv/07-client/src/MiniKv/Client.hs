-- ANCHOR: all
-- | A small client: send words, wait for one whole reply.
-- 小さなクライアント：単語を送り、答えが1つそろうまで待つ
module MiniKv.Client
  ( Connection
  , connectTo
  , disconnect
  , withConnection
  , request
  , receive
  , requestMany
  ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import MiniKv.Wire

-- ANCHOR: client
-- 🔌 A connection: the socket, and the bytes read but not used yet.
-- つながり：ソケットと、読んだけれどまだ使っていないバイト
data Connection = Connection Socket (IORef ByteString)

connectTo :: PortNumber -> IO Connection
connectTo port = do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
  Connection sock <$> newIORef BS.empty

disconnect :: Connection -> IO ()
disconnect (Connection sock _) = close sock

withConnection :: PortNumber -> (Connection -> IO a) -> IO a
withConnection port = bracket (connectTo port) disconnect

-- 📮 Send a request (always in the length-prefixed form) and read one
--    reply, waiting for more bytes as long as the parser asks.
-- 要求を（いつも長さつきの形で）送り、答えを1つ読む。
-- パーサーが「続きを」と言うあいだは、待つ
request :: Connection -> [ByteString] -> IO Wire
request conn@(Connection sock _) words' = do
  sendAll sock (render (WArray [ WBulk (Just w) | w <- words' ]))
  receive conn

-- ANCHOR: requestMany
-- 🚚 Pipelining: send many requests at once, then read all the
--    replies. One trip over the network instead of many.
-- パイプライン：要求をまとめて送り、答えをまとめて読む。
-- ネットワークの往復が、何回もから1回に
requestMany :: Connection -> [[ByteString]] -> IO [Wire]
requestMany conn@(Connection sock _) requests = do
  sendAll sock (foldMap (\ws -> render (WArray [ WBulk (Just w) | w <- ws ]))
                        requests)
  mapM (const (receive conn)) requests
-- ANCHOR_END: requestMany

-- 📬 Read one reply without sending anything (for subscribers, who
--    only listen).
-- 何も送らずに、答えを1つ読む（聞くだけの購読者のため）
receive :: Connection -> IO Wire
receive (Connection sock leftover) = readOne =<< readIORef leftover
  where
    readOne buffer = case parseWire buffer of
      Parsed reply rest -> writeIORef leftover rest >> pure reply
      Broken why        -> fail ("broken reply: " <> why)
      NeedMore -> do
        chunk <- recv sock 4096
        if BS.null chunk
          then fail "the server hung up"
          else readOne (buffer <> chunk)
-- ANCHOR_END: client
-- ANCHOR_END: all
