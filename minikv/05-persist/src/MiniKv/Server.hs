-- ANCHOR: all
-- | The TCP server, now with a log: commands that change the store are
--   written down, in the order they were applied.
-- TCP サーバー。ログ付き：ストアを変えたコマンドを、変えた順に書き残す
module MiniKv.Server
  ( Clock
  , systemClock
  , Env (..)
  , openEnv
  , runCommand
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
import Control.Concurrent.STM (TVar, atomically, newTVarIO, stateTVar)
import Control.Exception (bracket)
import Control.Monad (forever, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (doesFileExist)
import System.IO (Handle, IOMode (..), hClose, hFlush, openFile)

import MiniKv.Persist
import MiniKv.Protocol
import MiniKv.Store
import MiniKv.Types (Command, Millis, Reply (..))
import MiniKv.Wire

-- 🕰️ A clock: where "now" comes from.
-- 時計：「今」の出どころ
type Clock = IO Millis

systemClock :: Clock
systemClock = do
  seconds <- getPOSIXTime
  pure (floor (seconds * 1000))

-- ANCHOR: env
-- 🎒 Everything the server carries around, in one record.
-- サーバーが持ち回るものを、1つのレコードに
data Env = Env
  { clock    :: Clock
  , storeVar :: TVar Store
  , journal  :: Maybe (MVar Handle)  -- ^ the log, locked / 錠つきのログ
  }

-- 📂 Read the log (if any) into a store, and open it to append more.
--    Says how many entries were replayed.
-- ログ（があれば）を読んでストアにし、続きを足せるように開く。
-- 何件読み戻したかを答える
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
  pure (Env clk var lock, replayed)
-- ANCHOR_END: env

-- ANCHOR: run
-- ⚛️ Run one command. A command that changes the store takes the log's
--    lock, so that the store and the log change in the same order.
--    Reads never wait for the disk.
-- コマンドを1つ動かす。ストアを変えるコマンドは、ログの錠をとる。
-- ストアとログが、同じ順番で変わるように。読むだけなら、ディスクを待たない
runCommand :: Env -> [ByteString] -> IO Wire
runCommand env words' =
  case toCommand words' of
    Left message -> pure (WError ("ERR " <> encodeUtf8 message))
    Right command -> do
      now <- clock env
      reply <- case (journal env, toLogged now command) of
        (Just lock, Just logged) -> withMVar lock $ \handle -> do
          reply <- change now command
          unless (failed reply) $ do
            BS.hPut handle (encodeEntry logged)
            hFlush handle
          pure reply
        _ -> change now command
      pure (replyWire reply)
  where
    change :: Millis -> Command -> IO Reply
    change now command =
      atomically (stateTVar (storeVar env) (apply now command))
    failed (Failed _) = True
    failed _          = False
-- ANCHOR_END: run

-- 🧑‍💻 Talk to one client.
-- クライアント1人と話す
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
      Parsed words' rest -> do
        reply <- runCommand env words'
        sendAll sock (render reply)
        answerAll rest

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

-- 🧹 Forget expired keys now and then. (Not logged: the log has the
--    absolute deadlines, so replaying it forgets the same keys.)
-- ときどき、期限切れのキーを忘れる。（ログには書かない：ログには絶対の
-- 期限が書いてあるので、読み戻せば同じキーを忘れる）
sweeper :: Env -> IO ()
sweeper env = forever $ do
  threadDelay 100000
  now <- clock env
  atomically (stateTVar (storeVar env) (sweep now))

-- 🧪 For tests: a server on a free port, with the given clock and an
--    optional log file.
-- テスト用：空いているポートに、渡された時計と、ログのファイル（任意）で
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
