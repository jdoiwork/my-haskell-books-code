-- ANCHOR: all
-- | The broker as a TCP server. The pure core lives in a TVar; every
--   connection has a reader, a writer, and its consumers.
-- TCP サーバーとしてのブローカー。純粋な核は TVar の中。
-- つながりごとに、読む係・書く係・受け手たちがいる
module MiniMq.Server
  ( Env (..)
  , newEnv
  , step
  , await
  , answer
  , serveClient
  , withScope
  , listenOn
  , acceptLoop
  , withServer
  ) where

import Control.Concurrent (forkFinally, forkIO, killThread)
import Control.Concurrent.Async (async, cancel, race_)
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.IORef
import Network.Socket hiding (bind)
import qualified Network.Socket as Socket

import MiniMq.Broker
import MiniMq.Protocol
import MiniMq.Types
import MiniMq.Wire

-- ANCHOR: env
-- 🎒 What the server carries: the whole broker, in one TVar.
-- サーバーが持ち回るもの：ブローカー全体を、TVar に1つ
newtype Env = Env { brokerVar :: TVar Broker }

newEnv :: IO Env
newEnv = Env <$> newTVarIO emptyBroker
-- ANCHOR_END: env

-- ANCHOR: step
-- ⚛️ Run one step of the pure core on the shared broker. If the core
--    says no, throw the error: the whole transaction is undone.
-- 純粋な核の一歩を、みんなのブローカーで動かす。核が「だめ」と言ったら、
-- エラーを投げる。取引は丸ごと、なかったことになる
step :: TVar Broker -> (Broker -> Either BrokerError (a, Broker)) -> STM a
step var core = do
  broker <- readTVar var
  case core broker of
    Left err             -> throwSTM err
    Right (a, broker') -> a <$ writeTVar var broker'

-- ⏳ The same, but "nothing yet" means: sleep until the broker changes,
--    and try again. That is all `retry` does.
-- 同じだが、「まだ何もない」なら、ブローカーが変わるまで眠って、
-- やり直す。retry がしてくれるのは、それだけ
await :: TVar Broker -> (Broker -> Either BrokerError (Maybe a, Broker))
      -> STM a
await var core = step var core >>= maybe retry pure
-- ANCHOR_END: step

-- ANCHOR: answer
-- 📬 Answer one request (all but CONSUME), in one transaction.
-- 頼みごとに1つ答える（CONSUME 以外）。1つの取引で
answer :: TVar Broker -> Request -> STM Response
answer var request = attempt `catchSTM` (pure . refuse)
  where
    attempt = case request of
      DeclareQueue q        -> Ok <$ modifyTVar' var (declareQueue q)
      DeclareExchange e how -> Ok <$ modifyTVar' var (declareExchange e how)
      Bind e key q          -> Ok <$ step var (unit . bind e key q)
      Publish e m           -> sent <$> step var (publish e m)
      Consume q             -> Ok <$ step var (unit . checkQueue q)
    unit = fmap ((),)
    checkQueue q broker = broker <$ lookupQueue q broker
    sent (Routed qs) = RoutedTo (length qs)
    sent Unroutable  = NoRoute

refuse :: BrokerError -> Response
refuse (NoSuchQueue (QueueName q))       = Refused ("no such queue: " <> q)
refuse (NoSuchExchange (ExchangeName e)) = Refused ("no such exchange: " <> e)
-- ANCHOR_END: answer

-- ANCHOR: consumer
-- 📤 A consumer: forever take the next letter and put it in the outbox,
--    both in one transaction. A letter is never half taken.
-- 受け手：次の手紙を取って、送り箱に入れる。これを、ずっと。
-- 2つは1つの取引なので、手紙が「取られたのに、どこにもない」ことはない
consumer :: TVar Broker -> QueueName -> TQueue Response -> IO ()
consumer var q outbox = forever $ atomically $ do
  letter <- await var (takeNext q)
  writeTQueue outbox (Deliver letter)
-- ANCHOR_END: consumer

-- ANCHOR: serve
-- 🧑‍💻 One connection: a reader that answers, a writer that is the only
--    one to touch the socket, and the consumers it started. When the
--    client hangs up, everything started here stops with it.
-- 1つのつながり：答える読み係、ソケットにさわる唯一の書き係、
-- そして始めた受け手たち。クライアントが切ったら、ここで始めたものは
-- 全部いっしょに止まる
serveClient :: Env -> Conn -> IO ()
serveClient (Env var) conn = withScope $ \spawn -> do
  outbox <- newTQueueIO
  let tell = atomically . writeTQueue outbox
      readLoop = receive conn >>= \case
        Got request -> do
          reply <- atomically (answer var request)
          tell reply
          case (request, reply) of
            (Consume q, Ok) -> spawn (consumer var q outbox)
            _               -> pure ()
          readLoop
        Garbled bad -> tell (Refused ("cannot read: " <> bad))
        Closed      -> pure ()
      writeLoop = forever (atomically (readTQueue outbox) >>= send conn)
  race_ readLoop writeLoop
  atomically (flushTQueue outbox) >>= mapM_ (send conn)   -- the last words

-- 🧵 Threads started inside stop when the scope ends, however it ends.
-- 中で始めたスレッドは、範囲が終わると（どう終わっても）止まる
withScope :: ((IO () -> IO ()) -> IO a) -> IO a
withScope body =
  bracket (newIORef []) (\started -> readIORef started >>= mapM_ cancel) $
    \started -> body (\job -> async job >>= \a -> modifyIORef started (a :))
-- ANCHOR_END: serve

-- ANCHOR: listen
-- 👂 Listen on 127.0.0.1, and give every client its own thread.
-- 127.0.0.1 で待ち受け、クライアントごとにスレッドを1つ
listenOn :: PortNumber -> IO Socket
listenOn port = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  Socket.bind sock (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 64
  pure sock

acceptLoop :: Env -> Socket -> IO ()
acceptLoop env server = forever $ do
  (client, _) <- accept server
  conn <- newConn client
  forkFinally (serveClient env conn) (\_ -> close client)

-- 🧪 For tests: a server on a free port, stopped when the test ends.
-- テスト用：空いているポートで動き、テストが終わると止まるサーバー
withServer :: (PortNumber -> IO a) -> IO a
withServer action = do
  env <- newEnv
  bracket (listenOn 0) close $ \server -> do
    port <- socketPort server
    bracket (forkIO (acceptLoop env server)) killThread $ \_ ->
      action port
-- ANCHOR_END: listen
-- ANCHOR_END: all
