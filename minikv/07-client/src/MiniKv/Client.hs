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

import Control.Concurrent.MVar
import Control.Exception (bracket, mask, onException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Traversable (mapAccumM)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import MiniKv.Wire

-- ANCHOR: connection
-- 🔌 A connection: the socket, and the bytes read but not used yet.
--    The bytes live in an MVar, which is also the turn: only the one
--    holding them may talk. Nothing means "closed halfway: do not use".
-- つながり：ソケットと、読んだけれどまだ使っていないバイト。
-- バイトは MVar にしまい、それが順番の札にもなる：持っている人だけが
-- 話せる。Nothing は「途中で閉じた。もう使わない」
data Connection = Connection Socket (MVar (Maybe ByteString))
-- ANCHOR_END: connection

connectTo :: PortNumber -> IO Connection
connectTo port = do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
  Connection sock <$> newMVar (Just BS.empty)

disconnect :: Connection -> IO ()
disconnect (Connection sock _) = close sock

withConnection :: PortNumber -> (Connection -> IO a) -> IO a
withConnection port = bracket (connectTo port) disconnect

-- ANCHOR: exclusive
-- 🎫 Take the turn, talk, and give the turn back with the bytes left
--    over. If we are stopped halfway (an error, a `timeout`, `cancel`),
--    a reply may still be on its way. We cannot know where it ends,
--    so we close the connection: the next request fails, instead of
--    reading the late reply as its own.
-- 順番の札をとって話し、残ったバイトと一緒に札を返す。途中で止められたら
-- （エラー、timeout、cancel）、答えがまだ届く途中かもしれない。どこまでが
-- その答えかはわからないので、つながりを閉じる：次の要求は、遅れて届いた
-- 答えを自分の答えとして読まずに、失敗する
exclusive :: Connection -> (Socket -> ByteString -> IO (ByteString, a))
          -> IO a
exclusive (Connection sock turn) talk = mask $ \restore -> do
  state <- takeMVar turn
  case state of
    Nothing -> do
      putMVar turn Nothing
      fail "the connection was closed after an unfinished request"
    Just buffer -> do
      (rest, a) <- restore (talk sock buffer)
                     `onException` (close sock >> putMVar turn Nothing)
      putMVar turn (Just rest)
      pure a
-- ANCHOR_END: exclusive

-- ANCHOR: request
-- 📮 Send a request (always in the length-prefixed form) and read one
--    reply, waiting for more bytes as long as the parser asks.
-- 要求を（いつも長さつきの形で）送り、答えを1つ読む。
-- パーサーが「続きを」と言うあいだは、待つ
request :: Connection -> [ByteString] -> IO Wire
request conn words' = exclusive conn $ \sock buffer -> do
  sendAll sock (renderRequest words')
  readReply sock buffer

-- 📬 Read one reply without sending anything (for subscribers, who
--    only listen).
-- 何も送らずに、答えを1つ読む（聞くだけの購読者のため）
receive :: Connection -> IO Wire
receive conn = exclusive conn readReply

-- 📖 Read one whole reply; give back the bytes after it, too.
-- 答えを1つまるごと読む。そのうしろのバイトも返す
readReply :: Socket -> ByteString -> IO (ByteString, Wire)
readReply sock buffer = case parseWire buffer of
  Parsed reply rest -> pure (rest, reply)
  Broken why        -> fail ("broken reply: " <> why)
  NeedMore -> do
    chunk <- recv sock 4096
    if BS.null chunk
      then fail "the server hung up"
      else readReply sock (buffer <> chunk)

-- ✉️ Words as a request on the wire. / 単語を、回線の上の要求に
renderRequest :: [ByteString] -> ByteString
renderRequest words' = render (WArray [ WBulk (Just w) | w <- words' ])
-- ANCHOR_END: request

-- ANCHOR: requestMany
-- 🚚 Pipelining: send many requests at once, then read all the
--    replies. One trip over the network instead of many.
--    (`exclusive` keeps the turn; we meet it in a later section.)
-- パイプライン：要求をまとめて送り、答えをまとめて読む。
-- ネットワークの往復が、何回もから1回に
-- （exclusive は順番を守るもの。あとの節で見る）
requestMany :: Connection -> [[ByteString]] -> IO [Wire]
requestMany conn requests = exclusive conn $ \sock buffer -> do
  sendAll sock (foldMap renderRequest requests)
  mapAccumM (\rest _ -> readReply sock rest) buffer requests
-- ANCHOR_END: requestMany
-- ANCHOR_END: all
