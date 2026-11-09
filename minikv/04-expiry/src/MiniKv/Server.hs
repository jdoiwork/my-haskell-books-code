-- ANCHOR: all
-- | The TCP server: one thread per client, one shared store in a TVar.
-- TCP サーバー：クライアントごとに1つのスレッド、共有のストアは TVar に1つ
-- ANCHOR: head
module MiniKv.Server
  ( Clock
  , systemClock
  , runCommand
  , serveClient
  , listenOn
  , acceptLoop
  , sweeper
  , withServer
  , withServerAt
  ) where

import Control.Concurrent (forkFinally, threadDelay)
import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar,
                               writeTVar)
import Control.Exception (bracket)
import Control.Monad (forever, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import MiniKv.Protocol
import MiniKv.Store
import MiniKv.Types (Millis)
import MiniKv.Wire
-- ANCHOR_END: head

-- ANCHOR: run
-- 🕰️ A clock: where "now" comes from. The real one reads the system
--    time; tests can pass a clock they move by hand.
-- 時計：「今」の出どころ。本物はシステムの時刻を読む。
-- テストでは、手で進める時計を渡せる
type Clock = IO Millis

systemClock :: Clock
systemClock = do
  seconds <- getPOSIXTime
  pure (floor (seconds * 1000))

-- ⚛️ Run one command on the shared store, atomically, at the time
--    the clock says.
-- 共有のストアで、時計が言う時刻に、コマンドを1つまとめて動かす
runCommand :: Clock -> TVar Store -> [ByteString] -> IO Wire
runCommand clock storeVar words' =
  case toCommand words' of
    Left message -> pure (WError ("ERR " <> encodeUtf8 message))
    Right command -> do
      now <- clock
      reply <- atomically (stateTVar' storeVar (apply now command))
      pure (replyWire reply)
-- ANCHOR_END: run

-- 🧮 Like stateTVar, but the new state is worked out before it goes
--    into the TVar (stateTVar puts in the recipe, not the result).
-- stateTVar と同じだが、新しい状態を計算してから TVar に入れる
-- （stateTVar は、計算した結果ではなく、作り方のまま入れる）
stateTVar' :: TVar s -> (s -> (a, s)) -> STM a
stateTVar' var f = do
  (a, new) <- f <$> readTVar var
  writeTVar var $! new
  pure a

-- ANCHOR: client
-- 🧑‍💻 Talk to one client: read bytes, answer every complete request,
--    keep the rest for later, until the client hangs up.
-- クライアント1人と話す：バイトを読み、そろった要求に全部答え、
-- 残りはとっておく。クライアントが切るまで
serveClient :: Clock -> TVar Store -> Socket -> IO ()
serveClient clock storeVar sock = loop BS.empty
  where
    loop buffer = do
      chunk <- recv sock 4096
      unless (BS.null chunk) $          -- empty means hung up / 空なら切れた
        answerAll (buffer <> chunk)
    answerAll buffer = case readRequest buffer of
      NeedMore -> loop buffer
      Broken why ->                     -- tell, then close / 伝えて、閉じる
        sendAll sock (render (WError ("ERR protocol: " <> BC.pack why)))
      Parsed [] rest -> answerAll rest  -- an empty line / 空の行
      Parsed words' rest -> do
        reply <- runCommand clock storeVar words'
        sendAll sock (render reply)
        answerAll rest
-- ANCHOR_END: client

-- ANCHOR: listen
-- 👂 Open a listening socket on 127.0.0.1 (port 0 = any free port).
-- 127.0.0.1 で待ち受けるソケットを開く（ポート 0 は、空いているどれか）
listenOn :: PortNumber -> IO Socket
listenOn port = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 64
  pure sock

-- 🔁 Accept clients forever; each gets its own thread.
--    forkFinally closes the socket however the thread ends.
-- クライアントを受け付け続ける。1人ずつ、自分のスレッドを持つ。
-- forkFinally は、スレッドがどう終わっても、ソケットを閉じる
acceptLoop :: Clock -> TVar Store -> Socket -> IO ()
acceptLoop clock storeVar server = forever $ do
  (client, _) <- accept server
  forkFinally (serveClient clock storeVar client) (\_ -> close client)
-- ANCHOR_END: listen

-- ANCHOR: sweeper
-- 🧹 Ten times a second, forget the keys whose time has come, even if
--    nobody asks for them (or memory would fill with dead keys).
-- 1秒に10回、時刻が来たキーを忘れる。だれも聞かないキーでも
-- （そうしないと、死んだキーでメモリーがいっぱいになる）
sweeper :: Clock -> TVar Store -> IO ()
sweeper clock storeVar = forever $ do
  threadDelay 100000
  now <- clock
  atomically (stateTVar' storeVar (sweep now))
-- ANCHOR_END: sweeper

-- ANCHOR: withServer
-- 🧪 For tests: start a server (with its sweeper) on a free port, using
--    the given clock; run the action with the port; stop the server.
-- テスト用：渡された時計で、空いているポートにサーバー（と掃除係）を立て、
-- ポートを渡して動かし、止める
withServerAt :: Clock -> (PortNumber -> IO a) -> IO a
withServerAt clock action = do
  storeVar <- newTVarIO emptyStore
  bracket (listenOn 0) close $ \server -> do
    port <- socketPort server
    withAsync (acceptLoop clock storeVar server) $ \acceptor ->
      withAsync (sweeper clock storeVar) $ \sweeping -> do
        link acceptor
        link sweeping
        action port

withServer :: (PortNumber -> IO a) -> IO a
withServer = withServerAt systemClock
-- ANCHOR_END: withServer
-- ANCHOR_END: all

