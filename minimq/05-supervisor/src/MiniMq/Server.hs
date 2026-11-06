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
  , Reply (..)
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
import Control.Exception (bracket, finally)
import Control.Monad (forever)
import Data.IORef
import qualified Data.Text as T
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
-- 📬 Three kinds of reply: say something, stay quiet (ACK and friends,
--    when all is well, just like AMQP), or welcome a new consumer.
-- 返事は3種類：何か言う、黙っている（ACK などは、まちがいがなければ
-- 何も言わない。AMQP と同じ）、新しい受け手を迎える
data Reply
  = Say Response
  | Quiet
  | Joined ConsumerId QueueName Int

-- 📬 Answer one request, in one transaction.
-- 頼みごとに1つ答える。1つの取引で
answer :: TVar Broker -> Request -> STM Reply
answer var request = attempt `catchSTM` (pure . Say . refuse)
  where
    attempt = case request of
      DeclareQueue q how    -> Say Ok <$ modifyTVar' var (declareQueue q how)
      DeclareExchange e how -> Say Ok <$ modifyTVar' var
                                            (declareExchange e how)
      Bind e key q          -> Say Ok <$ change (bind e key q)
      Publish e m           -> Say . sent <$> step var (publish e m)
      Consume q limit       -> (\who -> Joined who q limit)
                                 <$> step var (subscribe q)
      Settle how number     -> Quiet <$ change (settle how number)
    change core = step var (fmap ((),) . core)
    sent (Routed qs) = RoutedTo (length qs)
    sent Unroutable  = NoRoute

refuse :: BrokerError -> Response
refuse err = Refused $ case err of
  NoSuchQueue (QueueName q)       -> "no such queue: " <> q
  NoSuchExchange (ExchangeName e) -> "no such exchange: " <> e
  UnknownTag (Tag n)              -> "unknown tag: " <> T.pack (show n)
-- ANCHOR_END: answer

-- ANCHOR: consumer
-- 📤 A consumer: take the next letter and put it in the outbox, in one
--    transaction. When it already holds `limit` letters, `takeNext` says
--    "nothing", so `await` sleeps until an ACK frees a place.
-- 受け手：次の手紙を取って送り箱へ。1つの取引で。もう limit 通
-- 持っていたら、takeNext が「何もない」と言うので、await は ACK で
-- 空きができるまで眠る
consumer :: TVar Broker -> ConsumerId -> Int -> QueueName -> TQueue Response
         -> IO ()
consumer var who limit q outbox = forever $ atomically $ do
  delivery <- await var (takeNext who limit q)
  writeTQueue outbox (Deliver delivery)
-- ANCHOR_END: consumer

-- ANCHOR: serve
-- 🧑‍💻 One connection. When the client hangs up, first its consumers
--    stop; then every letter they held without an answer goes back.
-- 1つのつながり。クライアントが切ったら、まず受け手たちが止まり、
-- そのあとで、返事をしないまま持っていた手紙が、全部戻る
serveClient :: Env -> Conn -> IO ()
serveClient (Env var) conn = do
  mine <- newIORef []
  serve mine `finally` giveBack mine
  where
    giveBack mine = do
      ids <- readIORef mine
      atomically (modifyTVar' var (\broker -> foldr abandon broker ids))
    serve mine = withScope $ \spawn -> do
      outbox <- newTQueueIO
      let tell = atomically . writeTQueue outbox
          readLoop = receive conn >>= \case
            Got request -> do
              reply <- atomically (answer var request)
              case reply of
                Say response -> tell response
                Quiet        -> pure ()
                Joined who q limit -> do
                  tell Ok
                  modifyIORef mine (who :)
                  spawn (consumer var who limit q outbox)
              readLoop
            Garbled bad -> tell (Refused ("cannot read: " <> bad))
            Closed      -> pure ()
          writeLoop = forever (atomically (readTQueue outbox) >>= send conn)
      race_ readLoop writeLoop
      atomically (flushTQueue outbox) >>= mapM_ (send conn)

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
