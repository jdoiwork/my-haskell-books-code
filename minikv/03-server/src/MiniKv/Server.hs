-- ANCHOR: all
-- | The TCP server: one thread per client, one shared store in a TVar.
-- TCP サーバー：クライアントごとに1つのスレッド、共有のストアは TVar に1つ
-- ANCHOR: head
module MiniKv.Server
  ( runCommand
  , serveClient
  , listenOn
  , acceptLoop
  , acceptNext
  , withServer
  , readPort
  , socketPort
  ) where

import Control.Concurrent (forkFinally, forkIOWithUnmask, killThread,
                           threadDelay)
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar,
                               writeTVar)
import Control.Exception (IOException, bracket, try)
import Control.Monad (forever, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isDigit)
import Data.Text.Encoding (encodeUtf8)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO (hPutStrLn, stderr)

import MiniKv.Protocol
import MiniKv.Store
import MiniKv.Wire
-- ANCHOR_END: head

-- ANCHOR: run
-- ⚛️ Run one command on the shared store, atomically. stateTVar wants
--    a function s -> (a, s): exactly the shape of `apply command`.
-- 共有のストアで、コマンドを1つ、まとめて（アトミックに）動かす。
-- stateTVar が欲しいのは s -> (a, s) の関数：`apply command` そのもの
runCommand :: TVar Store -> [ByteString] -> IO Wire
runCommand storeVar words' =
  case toCommand words' of
    Left message -> pure (WError ("ERR " <> encodeUtf8 message))
    Right command -> do
      reply <- atomically (stateTVar' storeVar (apply command))
      pure (replyWire reply)
-- ANCHOR_END: run

-- ANCHOR: strict
-- 🧮 Like stateTVar, but the new state is worked out before it goes
--    into the TVar (stateTVar puts in the recipe, not the result).
-- stateTVar と同じだが、新しい状態を計算してから TVar に入れる
-- （stateTVar は、計算した結果ではなく、作り方のまま入れる）
stateTVar' :: TVar s -> (s -> (a, s)) -> STM a
stateTVar' var f = do
  (a, new) <- f <$> readTVar var
  writeTVar var $! new
  pure a
-- ANCHOR_END: strict

-- ANCHOR: client
-- 🧑‍💻 Talk to one client: read bytes, answer every complete request,
--    keep the rest for later, until the client hangs up.
-- クライアント1人と話す：バイトを読み、そろった要求に全部答え、
-- 残りはとっておく。クライアントが切るまで
serveClient :: TVar Store -> Socket -> IO ()
serveClient storeVar sock = loop BS.empty
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
        reply <- runCommand storeVar words'
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

-- 🚪 Accept the next client. If the OS refuses (too many open files),
--    say so, wait a little, and try again: the server keeps running.
-- 次のクライアントを受け付ける。OS に断られたら（ファイルの数の上限など）、
-- 知らせて少し待ち、もう一度。サーバーは止めない
acceptNext :: Socket -> IO Socket
acceptNext server = do
  accepted <- try (accept server)
  case accepted of
    Right (client, _) -> pure client
    Left err -> do
      -- 「受け付けられませんでした。もう一度試します」
      hPutStrLn stderr ("minikv: accept failed, trying again: "
                        <> show (err :: IOException))
      threadDelay 100000
      acceptNext server

-- 🔢 A port number from the command line: digits only, 1 to 65535.
-- コマンドラインのポート番号：数字だけで、1 から 65535
readPort :: String -> Maybe PortNumber
readPort s
  | null s || length s > 5 || not (all isDigit s) = Nothing
  | n < 1 || n > 65535                            = Nothing
  | otherwise                                     = Just (fromInteger n)
  where
    n = read s :: Integer

-- 🔁 Accept clients forever; each gets its own thread.
--    forkFinally closes the socket however the thread ends.
-- クライアントを受け付け続ける。1人ずつ、自分のスレッドを持つ。
-- forkFinally は、スレッドがどう終わっても、ソケットを閉じる
acceptLoop :: TVar Store -> Socket -> IO ()
acceptLoop storeVar server = forever $ do
  client <- acceptNext server
  forkFinally (serveClient storeVar client) (\_ -> close client)
-- ANCHOR_END: listen

-- ANCHOR: withServer
-- 🧪 For tests: start a server on a free port, run the action with the
--    port, then stop the server.
-- テスト用：空いているポートでサーバーを立て、ポートを渡して動かし、止める
withServer :: (PortNumber -> IO a) -> IO a
withServer action = do
  storeVar <- newTVarIO emptyStore
  bracket (listenOn 0) close $ \server -> do
    port <- socketPort server
    -- forkIOWithUnmask: the receptionist runs unmasked, though it is
    -- started inside bracket's "take" (which masks).
    -- 受付係は、マスクを外して動く（bracket の「取る」の中で立てても）
    let receptionist = forkIOWithUnmask $ \unmask ->
          unmask (acceptLoop storeVar server)
    bracket receptionist killThread $ \_ ->
      action port
-- ANCHOR_END: withServer
-- ANCHOR_END: all

