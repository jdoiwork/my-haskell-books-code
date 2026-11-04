-- ANCHOR: all
-- | The TCP server, with a log, blocking pops and publish/subscribe.
-- TCP サーバー。ログ、待つ取り出し、Pub/Sub 付き
module MiniKv.Server
  ( Clock
  , systemClock
  , Env (..)
  , openEnv
  , runCommand
  , blockingPop
  , publish
  , subscribe
  , serveClient
  , listenOn
  , acceptLoop
  , sweeper
  , withServer
  , withServerAt
  , withServerLog
  ) where

import Control.Concurrent (MVar, forkFinally, forkIO, killThread,
                           newMVar, threadDelay, withMVar)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Exception (bracket, finally)
import Control.Monad (forM_, forever, unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (doesFileExist)
import System.IO (Handle, IOMode (..), hClose, hFlush, openFile)

import MiniKv.Persist
import MiniKv.Protocol
import MiniKv.Request
import MiniKv.Store
import MiniKv.Types
import MiniKv.Wire

-- 🕰️ A clock: where "now" comes from.
-- 時計：「今」の出どころ
type Clock = IO Millis

systemClock :: Clock
systemClock = do
  seconds <- getPOSIXTime
  pure (floor (seconds * 1000))

-- ANCHOR: env
-- 🎒 Everything the server carries around. New: a broadcast channel
--    for messages, and how many listen to each channel.
-- サーバーが持ち回るもの。新しく：メッセージを配る放送用のチャンネルと、
-- それぞれのチャンネルを聞いている人数
data Env = Env
  { clock     :: Clock
  , storeVar  :: TVar Store
  , journal   :: Maybe (MVar Handle)
  , hub       :: TChan (Text, Text)
  , listeners :: TVar (Map Text Int)
  }
-- ANCHOR_END: env

-- 📂 Read the log (if any) and open it to append more.
-- ログ（があれば）を読み、続きを足せるように開く
openEnv :: Clock -> Maybe FilePath -> IO (Env, Replayed)
openEnv clk logPath = do
  replayed <- case logPath of
    Nothing   -> pure (Replayed emptyStore 0 Nothing)
    Just path -> do
      exists <- doesFileExist path
      if exists then replay <$> BS.readFile path
                else pure (Replayed emptyStore 0 Nothing)
  var <- newTVarIO (replayedStore replayed)
  lock <- traverse (\path -> openFile path AppendMode >>= newMVar) logPath
  channel <- newBroadcastTChanIO
  counts <- newTVarIO Map.empty
  pure (Env clk var lock channel counts, replayed)

-- ⚛️ Run one plain command (logging it if it changes the store).
-- ふつうのコマンドを1つ動かす（ストアを変えるなら、ログに書く）
runCommand :: Env -> Command -> IO Wire
runCommand env command = do
  now <- clock env
  reply <- case (journal env, toLogged now command) of
    (Just lock, Just logged) -> withMVar lock $ \handle -> do
      reply <- change now
      unless (failed reply) $ do
        BS.hPut handle (encodeEntry logged)
        hFlush handle
      pure reply
    _ -> change now
  pure (replyWire reply)
  where
    change now = atomically (stateTVar (storeVar env) (apply now command))
    failed (Failed _) = True
    failed _          = False

-- ANCHOR: blpop
-- ⏳ BLPOP: wait until the list has something (or the time is up),
--    then pop it with a plain LPOP. If another client took it first,
--    wait again. STM's `retry` sleeps until the store changes.
-- BLPOP：リストに何か入るまで（か時間切れまで）待ち、ふつうの LPOP で
-- 取り出す。先にほかのクライアントに取られたら、また待つ。
-- STM の retry は、ストアが変わるまで眠る
blockingPop :: Env -> Key -> Int -> IO Wire
blockingPop env key seconds = do
  timeUp <- if seconds == 0 then newTVarIO False
                            else registerDelay (seconds * 1000000)
  let attempt = do
        ready <- atomically (waitForItem `orElse` waitForTimeout timeUp)
        if not ready then pure (WBulk Nothing) else do
          reply <- runCommand env (LPop key)
          case reply of
            WBulk Nothing -> attempt    -- someone was faster / 先を越された
            other         -> pure other
  attempt
  where
    waitForItem = do
      store <- readTVar (storeVar env)
      case fst (apply 0 (LLen key) store) of
        Number 0 -> retry                     -- nothing yet / まだ空
        _        -> pure True       -- an item, or an error / 何かある
    waitForTimeout timeUp = do
      done <- readTVar timeUp
      check done
      pure False
-- ANCHOR_END: blpop

-- ANCHOR: pubsub
-- 📣 PUBLISH: put the message on the broadcast channel; answer how many
--    listen to that channel.
-- PUBLISH：放送用のチャンネルにメッセージを置き、そのチャンネルを
-- 聞いている人数を答える
publish :: Env -> Text -> Text -> IO Wire
publish env channel message = atomically $ do
  writeTChan (hub env) (channel, message)
  counts <- readTVar (listeners env)
  pure (WInt (Map.findWithDefault 0 channel counts))

-- 👂 SUBSCRIBE: from now on this connection only receives messages.
--    Two things run side by side; when the client hangs up, `race_`
--    stops the other, and `finally` takes us off the listener counts.
-- SUBSCRIBE：このつながりは、これからメッセージを受け取るだけになる。
-- 2つのことを同時に動かし、クライアントが切ったら race_ がもう片方を止め、
-- finally が人数から外す
subscribe :: Env -> Socket -> [Text] -> IO ()
subscribe env sock channels = do
  mine <- atomically $ do
    forM_ channels $ \c ->
      modifyTVar' (listeners env) (Map.insertWith (+) c 1)
    dupTChan (hub env)
  forM_ (zip [1 :: Int ..] channels) $ \(i, c) ->
    sendAll sock (render (WArray [bulk "subscribe", bulk' c, WInt i]))
  race_ (deliver mine) waitForHangUp `finally` leave
  where
    deliver mine = forever $ do
      (c, m) <- atomically (readTChan mine)
      when (c `elem` channels) $
        sendAll sock (render (WArray [bulk "message", bulk' c, bulk' m]))
    waitForHangUp = do
      chunk <- recv sock 4096
      unless (BS.null chunk) waitForHangUp
    leave = atomically $ forM_ channels $ \c ->
      modifyTVar' (listeners env) (Map.adjust (subtract 1) c)
    bulk s  = WBulk (Just s)
    bulk' t = WBulk (Just (encodeUtf8 t))
-- ANCHOR_END: pubsub

-- ANCHOR: client
-- 🧑‍💻 Talk to one client: each request goes to the right place.
-- クライアント1人と話す：要求ごとに、ふさわしい場所へ
serveClient :: Env -> Socket -> IO ()
serveClient env sock = loop BS.empty
  where
    loop buffer = do
      chunk <- recv sock 4096
      unless (BS.null chunk) $
        answerAll (buffer <> chunk)
    answerAll buffer = case readRequest buffer of
      NeedMore -> loop buffer
      Broken why ->
        sendAll sock (render (WError ("ERR protocol: " <> BC.pack why)))
      Parsed [] rest -> answerAll rest
      Parsed words' rest -> case toRequest words' of
        Left message -> do
          sendAll sock (render (WError ("ERR " <> encodeUtf8 message)))
          answerAll rest
        Right (Subscribe channels) ->
          subscribe env sock channels     -- this connection now listens
        Right other -> do
          reply <- case other of
            Plain command   -> runCommand env command
            BLPop key secs  -> blockingPop env key secs
            Publish c m     -> publish env c m
          sendAll sock (render reply)
          answerAll rest
-- ANCHOR_END: client

-- 👂 Listen on 127.0.0.1.
-- 127.0.0.1 で待ち受ける
listenOn :: PortNumber -> IO Socket
listenOn port = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 64
  pure sock

-- 🔁 Accept clients forever.
-- クライアントを受け付け続ける
acceptLoop :: Env -> Socket -> IO ()
acceptLoop env server = forever $ do
  (client, _) <- accept server
  forkFinally (serveClient env client) (\_ -> close client)

-- 🧹 Forget expired keys now and then.
-- ときどき、期限切れのキーを忘れる
sweeper :: Env -> IO ()
sweeper env = forever $ do
  threadDelay 100000
  now <- clock env
  atomically (stateTVar (storeVar env) (sweep now))

-- 🧪 For tests.
-- テスト用
withServerLog :: Clock -> Maybe FilePath -> (PortNumber -> IO a) -> IO a
withServerLog clk logPath action =
  bracket (openEnv clk logPath) (closeEnv . fst) $ \(env, _) ->
    bracket (listenOn 0) close $ \server -> do
      port <- socketPort server
      bracket (forkIO (acceptLoop env server)) killThread $ \_ ->
        bracket (forkIO (sweeper env)) killThread $ \_ ->
          action port
  where
    closeEnv env = traverse (\lock -> withMVar lock hClose) (journal env)

withServerAt :: Clock -> (PortNumber -> IO a) -> IO a
withServerAt clk = withServerLog clk Nothing

withServer :: (PortNumber -> IO a) -> IO a
withServer = withServerAt systemClock

-- ANCHOR_END: all
