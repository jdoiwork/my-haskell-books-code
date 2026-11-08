-- ANCHOR: all
-- | The broker as a TCP server. The pure core lives in a TVar; every
--   connection has a reader, a writer, and its consumers.
-- TCP サーバーとしてのブローカー。純粋な核は TVar の中。
-- つながりごとに、読む係・書く係・受け手たちがいる
-- ANCHOR: imports
module MiniMq.Server
  ( Env (..)
  , withEnv
  , step
  , modify
  , await
  , Reply (..)
  , answer
  , serveClient
  , withScope
  , listenOn
  , acceptLoop
  , withGuests
  , withServer
  , withServerAt
  ) where

import Control.Concurrent.Async (async, cancel, concurrently_, link,
                                 withAsync)
import Control.Concurrent.STM
import Control.Exception (bracket, finally, mask)
import Control.Monad (forever)
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Network.Socket hiding (bind)
import qualified Network.Socket as Socket
import System.IO (IOMode (..), withFile)

import MiniMq.Broker
import MiniMq.Journal
import MiniMq.Protocol
import MiniMq.Types
import MiniMq.Wire
-- ANCHOR_END: imports

-- ANCHOR: env
-- 🎒 What the server carries: the broker, and now its journal.
-- サーバーが持ち回るもの：ブローカーと、新しく日誌
data Env = Env
  { brokerVar :: TVar Broker
  , journal   :: Journal
  }

-- 📂 Read the journal (if any), build the broker from it, write it again
--    short, and keep a writer running while the server lives. If the
--    writer falls (a full disk, say), `link` makes the server fall too,
--    with the reason, instead of waiting for the disk forever.
-- 日誌（があれば）を読んでブローカーを作り直し、短く書き直してから、
-- サーバーが動いているあいだ、書き係を動かし続ける。書き係が倒れたら
-- （ディスクがいっぱい、など）、link で、サーバーも理由つきで倒れる。
-- 来ない書き込みを、いつまでも待ったりしない
withEnv :: Maybe FilePath -> (Env -> IO a) -> IO a
withEnv path action = do
  broker <- maybe (pure emptyBroker) (fmap replay . loadJournal) path
  mapM_ (\file -> rewriteJournal file (compact broker)) path
  env <- Env <$> newTVarIO broker <*> newJournal
  let run target =
        withAsync (writer target (journal env)) $ \scribe -> do
          link scribe
          action env
  case path of
    Nothing   -> run Nothing
    Just file -> withFile file AppendMode (run . Just)
-- ANCHOR_END: env

-- ANCHOR: step
-- ⚛️ Run one step of the pure core on the shared broker, and hand the
--    notes it wrote to the journal (as one line), in the same
--    transaction. If the core
--    says no, throw the error: the whole transaction is undone.
-- 純粋な核の一歩を、みんなのブローカーで動かし、核が書いた覚え書きを、
-- 同じ取引の中で日誌に手渡す。核が「だめ」と言ったら、エラーを投げる。
-- 取引は丸ごと、なかったことになる
step :: Env -> (Broker -> Either BrokerError (a, Broker)) -> STM a
step env core = do
  broker <- readTVar (brokerVar env)
  case core broker of
    Left err -> throwSTM err
    Right (a, changed) -> do
      let (entries, clean) = takeNotes changed
      writeTVar (brokerVar env) clean
      handIn (journal env) entries
      pure a

modify :: Env -> (Broker -> Broker) -> STM ()
modify env change = step env (\broker -> Right ((), change broker))

-- ⏳ The same, but "nothing yet" means: sleep until the broker changes,
--    and try again. That is all `retry` does.
-- 同じだが、「まだ何もない」なら、ブローカーが変わるまで眠って、
-- やり直す。retry がしてくれるのは、それだけ
await :: Env -> (Broker -> Either BrokerError (Maybe a, Broker)) -> STM a
await env core = step env core >>= maybe retry pure

-- 💾 Wait until everything handed in so far is on disk.
-- ここまでに手渡した行が、全部ディスクに届くまで待つ
safelyWritten :: Env -> IO ()
safelyWritten env = do
  ticket <- readTVarIO (handedIn (journal env))
  atomically (readTVar (onDisk (journal env)) >>= check . (>= ticket))
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

-- 📬 Answer one request, in one transaction. `mine` are this line's
--    consumers: an answer may only settle a letter one of them holds.
-- 頼みごとに1つ答える。1つの取引で。mine は、このつながりの受け手たち。
-- 返事で片付けられるのは、その誰かが持っている手紙だけ
answer :: Env -> [ConsumerId] -> Request -> STM Reply
answer env mine request = attempt `catchSTM` (pure . Say . refuse)
  where
    attempt = case request of
      DeclareQueue q how    -> Say Ok <$ modify env (declareQueue q how)
      DeclareExchange e how -> Say Ok <$ modify env (declareExchange e how)
      Bind e key q          -> Say Ok <$ change (bind e key q)
      Publish e m           -> Say . sent <$> step env (publish e m)
      Consume q limit       -> (\who -> Joined who q limit)
                                 <$> step env (subscribe q)
      Settle how number     -> Quiet <$ change (settle mine how number)
    change core = step env (fmap ((),) . core)
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
consumer :: Env -> ConsumerId -> Int -> QueueName -> Outbox -> IO ()
consumer env who limit q outbox = forever $ atomically $ do
  delivery <- await env (takeNext who limit q)
  post outbox (Deliver delivery)
-- ANCHOR_END: consumer

-- ANCHOR: serve
-- 📦 The outbox: what to say, and at the very end, `Nothing` ("that is
--    all"). Only the writer takes from it.
-- 送り箱：言うことと、いちばん最後に Nothing（「これでおしまい」）。
-- 取り出すのは、書き係だけ
type Outbox = TQueue (Maybe Response)

post :: Outbox -> Response -> STM ()
post outbox = writeTQueue outbox . Just

-- 🧑‍💻 One connection. When the client hangs up, first its consumers
--    stop; the writer says what is left; then every letter they held
--    without an answer goes back.
-- 1つのつながり。クライアントが切ったら、まず受け手たちが止まり、
-- 書き係が残りを言い終える。そのあとで、返事をしないまま持っていた
-- 手紙が、全部戻る
serveClient :: Env -> Conn -> IO ()
serveClient env conn = do
  mine <- newIORef []
  serve mine `finally` giveBack mine
  where
    giveBack mine = do
      ids <- readIORef mine
      atomically (modify env (\broker -> foldr abandon broker ids))
    serve mine = do
      outbox <- newTQueueIO
      let tell = atomically . post outbox
          readLoop spawn = receive conn >>= \case
            Got request -> do
              ids   <- readIORef mine       -- only this reader writes it
              reply <- atomically (answer env ids request)
              safelyWritten env      -- 💾 say nothing before it is on disk
              case reply of
                Say response -> tell response
                Quiet        -> pure ()
                Joined who q limit -> do
                  tell Ok
                  modifyIORef mine (who :)
                  spawn (consumer env who limit q outbox)
              readLoop spawn
            Garbled bad -> tell (Refused ("cannot read: " <> bad))
            Closed      -> pure ()
          writeLoop = atomically (readTQueue outbox) >>= \case
            Just response -> send conn response >> writeLoop
            Nothing       -> pure ()                  -- that is all
          reader = do
            withScope readLoop            -- the consumers stop here
            atomically (writeTQueue outbox Nothing)   -- the last words
      concurrently_ reader writeLoop

-- 🧵 Threads started inside stop when the scope ends, however it ends.
--    Starting one and writing it down happen under `mask`, so no
--    exception can slip in between; the job itself runs unmasked.
-- 中で始めたスレッドは、範囲が終わると（どう終わっても）止まる。
-- 始めることと書き留めることは mask の中でするので、そのあいだに
-- 例外は割りこめない。仕事そのものは、mask を外して動く
withScope :: ((IO () -> IO ()) -> IO a) -> IO a
withScope body =
  bracket (newIORef []) (\started -> readIORef started >>= mapM_ cancel) $
    \started -> body $ \job -> mask $ \restore -> do
      thread <- async (restore job)
      modifyIORef started (thread :)
-- ANCHOR_END: serve

-- ANCHOR: listen
-- 👂 Listen on 127.0.0.1, and give every client its own thread. The
--    thread is started by `welcome`, so it lives in the server's scope.
-- 127.0.0.1 で待ち受け、クライアントごとにスレッドを1つ。スレッドは
-- welcome で始めるので、サーバーの範囲の中で動く
listenOn :: PortNumber -> IO Socket
listenOn port = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  Socket.bind sock (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 64
  pure sock

acceptLoop :: Env -> Socket -> (IO () -> IO ()) -> IO ()
acceptLoop env server welcome = forever $ do
  (client, _) <- accept server
  conn <- newConn client
  welcome (serveClient env conn `finally` close client)

-- 🚪 Like `withScope`, for guests who come and go. When the scope ends,
--    every guest still here is stopped (and its socket closed). A guest
--    who leaves crosses itself out, so a server that runs for weeks
--    does not keep a list of everyone who ever came. It waits until the
--    door has written it down (under `mask`, that comes right away).
-- withScope の、来ては帰る客の版。範囲が終わると、まだいる客を全部
-- 止める（ソケットも閉じる）。帰る客は自分で名前を消すので、何週間も
-- 動くサーバーが、来た客全員の一覧を抱えこまない。消す前に、戸口が
-- 書き留めるのを待つ（mask の中なので、すぐに書かれる）
withGuests :: ((IO () -> IO ()) -> IO a) -> IO a
withGuests body = do
  guests  <- newTVarIO Map.empty
  tickets <- newIORef (0 :: Int)
  let leave n = atomically $ do
        here <- readTVar guests
        check (Map.member n here)
        writeTVar guests (Map.delete n here)
      welcome job = mask $ \restore -> do
        n <- atomicModifyIORef' tickets (\k -> (k + 1, k))
        guest <- async (restore job `finally` leave n)
        atomically (modifyTVar' guests (Map.insert n guest))
      stopAll = readTVarIO guests >>= mapM_ cancel
  body welcome `finally` stopAll

-- 🧪 For tests: a server on a free port, stopped when the test ends.
--    Stopping it cleans up as usual (the journal file is closed), so
--    starting it again is a restart, not a pulled plug.
-- テスト用：空いているポートで動き、テストが終わると止まるサーバー。
-- 止めるときは、いつもどおり後片付けをする（日誌のファイルも閉じる）。
-- だから、もう一度起動するのは、電源を抜いたあとではなく、再起動
withServerAt :: Maybe FilePath -> (PortNumber -> IO a) -> IO a
withServerAt path action =
  withEnv path $ \env ->
    bracket (listenOn 0) close $ \server -> do
      port <- socketPort server
      withGuests $ \welcome ->
        withAsync (acceptLoop env server welcome) $ \acceptor -> do
          link acceptor
          action port

withServer :: (PortNumber -> IO a) -> IO a
withServer = withServerAt Nothing
-- ANCHOR_END: listen
-- ANCHOR_END: all
