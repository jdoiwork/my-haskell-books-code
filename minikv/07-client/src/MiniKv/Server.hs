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
  , withGuests
  , sweeper
  , withServer
  , withServerAt
  , withServerLog
  ) where

import Control.Concurrent (MVar, newMVar, threadDelay, threadWaitRead,
                           withMVar)
import Control.Concurrent.Async (async, cancel, link, race, race_,
                                 withAsync)
import Control.Concurrent.STM
import Control.Exception (IOException, SomeException, bracket,
                          displayException, finally, fromException,
                          mask, throwIO, try)
import Control.Monad (forM_, forever, unless, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef,
                   writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (copyFile, doesFileExist, renameFile)
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
  { clock      :: Clock
  , storeVar   :: TVar Store
  , journal    :: Maybe (MVar Handle)
  , logFailure :: IORef (Maybe ByteString)
  , hub        :: TChan (Text, Text)
  , listeners  :: TVar (Map Text Int)
  }
-- ANCHOR_END: env

-- ANCHOR: openEnv
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
  forM_ logPath $ \path -> repairLog path replayed
  var <- newTVarIO (replayedStore replayed)
  lock <- traverse (\path -> openFile path AppendMode >>= newMVar) logPath
  failure <- newIORef Nothing
  channel <- newBroadcastTChanIO
  counts <- newTVarIO Map.empty
  pure (Env clk var lock failure channel counts, replayed)
-- ANCHOR_END: openEnv

-- ANCHOR: repair
-- 🩹 If reading stopped early, the log has a broken entry (often the
--    last one, cut short). Entries added after it would never be read
--    back: reading stops there again. So keep a copy of the old log,
--    and put a fresh log of what we could read in its place (renaming
--    a file is all or nothing).
-- 読み戻しが途中で止まったら、ログには壊れた1件がある（たいていは、
-- 途中で切れた最後の1件）。そのあとに足したものは、次も読み戻せない
-- （また、そこで止まる）。だから、古いログの写しを残し、読めた分から
-- 作った新しいログに置き換える（ファイルの名前の付け替えは、全部か無しか）
repairLog :: FilePath -> Replayed -> IO ()
repairLog path replayed = case problem replayed of
  Nothing -> pure ()
  Just _  -> do
    copyFile path (path <> ".broken")
    BS.writeFile (path <> ".new") (rewriteLog (replayedStore replayed))
    renameFile (path <> ".new") path
-- ANCHOR_END: repair

-- ANCHOR: run
-- ⚛️ Run one plain command (logging it if it changes the store).
-- ふつうのコマンドを1つ動かす（ストアを変えるなら、ログに書く）
runCommand :: Env -> Command -> IO Wire
runCommand env command = do
  now <- clock env
  outcome <- case toLogged now command of
    Nothing -> Right . fst . apply now command <$> readTVarIO (storeVar env)
    Just _  -> changeAndLog env (logStep now command)
  pure (either refusal replyWire outcome)
-- ANCHOR_END: run

-- 🚫 The answer to a change we refuse (like Redis's MISCONF).
-- 断るときの答え（Redis の MISCONF のように）
refusal :: ByteString -> Wire
refusal why = WError ("MISCONF Errors writing to the log: " <> why)

-- ✍️ Holding the log's lock, work out the new store, write what the
--    step says to the log, and only when that worked, put the new
--    store in. No other writer comes in between, so the store and the
--    log change in the same order. If writing fails, say why, and
--    refuse every change from then on (reads go on).
-- ログの錠をとったまま、新しいストアを計算し、step が言うものをログに
-- 書き、書けたときだけ、新しいストアを入れる。ほかの書き手は割りこまない
-- ので、ストアとログは同じ順番で変わる。書けなければ理由を答え、
-- それからあとの変更を、全部断る（読むのは続けられる）
changeAndLog :: Env -> (Store -> ((a, [Command]), Store))
             -> IO (Either ByteString a)
changeAndLog env step = case journal env of
  Nothing   -> Right . fst <$> atomically (stateTVar' (storeVar env) step)
  Just lock -> withMVar lock $ \handle -> do
    failed <- readIORef (logFailure env)
    case failed of
      Just why -> pure (Left why)
      Nothing  -> do
        store <- readTVarIO (storeVar env)
        let ((a, entries), store') = step store
        outcome <- try @SomeException $ do
          unless (null entries) $ do
            BS.hPut handle (foldMap encodeEntry entries)
            hFlush handle
          atomically (writeTVar (storeVar env) $! store')
        case outcome of
          Right () -> pure (Right a)
          Left err -> do
            -- The log and the store may differ now: no more changes.
            -- ログとストアが食い違ったかもしれない：もう変えない
            let why = BC.pack (filter (`notElem` ("\r\n" :: String))
                                      (displayException err))
            writeIORef (logFailure env) (Just why)
            case fromException @IOException err of
              Just _  -> pure (Left why)
              Nothing -> throwIO err    -- e.g. cancel / cancel など

-- 🧮 Like stateTVar, but the new state is worked out before it goes
--    into the TVar (stateTVar puts in the recipe, not the result).
-- stateTVar と同じだが、新しい状態を計算してから TVar に入れる
-- （stateTVar は、計算した結果ではなく、作り方のまま入れる）
stateTVar' :: TVar s -> (s -> (a, s)) -> STM a
stateTVar' var f = do
  (a, new) <- f <$> readTVar var
  writeTVar var $! new
  pure a


-- ANCHOR: blpop
-- ⏳ BLPOP: wait until the list has something (or the time is up),
--    then pop it with a plain LPOP. If another client took it first,
--    wait again. STM's `retry` sleeps until the store changes.
--    While waiting, it also watches the client. If the client hangs
--    up, it stops and takes nothing (Nothing). Bytes the client sends
--    meanwhile are kept, and given back with the reply.
-- BLPOP：リストに何か入るまで（か時間切れまで）待ち、ふつうの LPOP で
-- 取り出す。先にほかのクライアントに取られたら、また待つ。
-- STM の retry は、ストアが変わるまで眠る。
-- 待つあいだ、クライアントも見張る。クライアントが切ったら、何も取らずに
-- やめる（Nothing）。そのあいだに届いたバイトは取っておき、答えと一緒に返す
blockingPop :: Env -> Socket -> Key -> Int -> IO (Maybe (Wire, ByteString))
blockingPop env sock key seconds = do
  timeUp <- if seconds == 0 then newTVarIO False
                            else registerDelay (seconds * 1000000)
  let attempt early = do
        woke <- race waitForClient
                     (atomically (waitForItem `orElse` waitForTimeout timeUp))
        case woke of
          Left () -> do
            chunk <- recv sock 4096
            if BS.null chunk
              then pure Nothing                 -- hung up / 切られた
              else attempt (early <> chunk)     -- keep it / 取っておく
          Right False -> pure (Just (WBulk Nothing, early))
          Right True  -> do
            reply <- runCommand env (LPop key)
            case reply of
              -- Someone was faster. / 先を越された
              WBulk Nothing -> attempt early
              other         -> pure (Just (other, early))
  attempt BS.empty
  where
    -- Wait until the client sent something or hung up, reading nothing.
    -- クライアントが何か送るか、切るまで待つ（読まずに）
    waitForClient = withFdSocket sock (threadWaitRead . fromIntegral)
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
--    `bracket` puts us on the listener counts and takes us off again,
--    however we end. Inside, two things run side by side; when the
--    client hangs up, `race_` stops the other.
-- SUBSCRIBE：このつながりは、これからメッセージを受け取るだけになる。
-- bracket が人数に足し、どんな終わり方でも、人数から外す。中では
-- 2つのことを同時に動かし、クライアントが切ったら race_ がもう片方を止める
subscribe :: Env -> Socket -> [Text] -> IO ()
subscribe env sock channels =
  bracket enter (const leave) $ \mine -> do
    forM_ (zip [1 :: Int ..] channels) $ \(i, c) ->
      sendAll sock (render (WArray [bulk "subscribe", bulk' c, WInt i]))
    race_ (deliver mine) waitForHangUp
  where
    enter = atomically $ do
      forM_ channels $ \c ->
        modifyTVar' (listeners env) (Map.insertWith (+) c 1)
      dupTChan (hub env)
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
        Right (BLPop key secs) -> do
          waited <- blockingPop env sock key secs
          case waited of
            Nothing             -> pure ()  -- the client left / 帰った
            Just (reply, early) -> do
              sendAll sock (render reply)
              answerAll (rest <> early)
        Right other -> do
          reply <- case other of
            Plain command   -> runCommand env command
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

-- 🔁 Accept clients forever. Each client's thread is started by
--    `welcome`, so it lives in the server's scope.
-- クライアントを受け付け続ける。客のスレッドは welcome で始めるので、
-- サーバーの範囲の中で動く
acceptLoop :: Env -> Socket -> (IO () -> IO ()) -> IO ()
acceptLoop env server welcome = forever $ do
  (client, _) <- accept server
  welcome (serveClient env client `finally` close client)

-- 🚪 Start each guest in a scope. When the scope ends, every guest
--    still here is stopped (and its socket closed). A guest who leaves
--    crosses itself out, so a server that runs for weeks does not keep
--    a list of everyone who ever came. It waits until the door has
--    written it down (under `mask`, that comes right away).
-- 客を、範囲の中で始める。範囲が終わると、まだいる客を全部止める
-- （ソケットも閉じる）。帰る客は自分で名前を消すので、何週間も動く
-- サーバーが、来た客全員の一覧を抱えこまない。消す前に、戸口が
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

-- 🧹 Forget expired keys now and then, and write them in the log.
-- ときどき、期限切れのキーを忘れ、ログにも書く
sweeper :: Env -> IO ()
sweeper env = forever $ do
  threadDelay 100000
  now <- clock env
  -- Refused (the log broke)? The keys stay, but GET still hides them.
  -- 断られた（ログが壊れた）？ キーは残るが、GET からは見えない
  void (changeAndLog env (sweepStep now))

-- 🧪 For tests.
-- テスト用
withServerLog :: Clock -> Maybe FilePath -> (PortNumber -> IO a) -> IO a
withServerLog clk logPath action =
  bracket (openEnv clk logPath) (closeEnv . fst) $ \(env, _) ->
    bracket (listenOn 0) close $ \server -> do
      port <- socketPort server
      withGuests $ \welcome ->
        withAsync (acceptLoop env server welcome) $ \acceptor ->
          withAsync (sweeper env) $ \sweeping -> do
            link acceptor
            link sweeping
            action port
  where
    closeEnv env = traverse (\lock -> withMVar lock hClose) (journal env)

withServerAt :: Clock -> (PortNumber -> IO a) -> IO a
withServerAt clk = withServerLog clk Nothing

withServer :: (PortNumber -> IO a) -> IO a
withServer = withServerAt systemClock

-- ANCHOR_END: all
