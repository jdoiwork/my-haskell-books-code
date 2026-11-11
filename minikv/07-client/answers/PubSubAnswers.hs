-- ANCHOR: all
-- ANCHOR: head
-- | Answers to the training of "Pub/Sub and blocking": UNSUBSCRIBE, and
--   BLPOP on many keys. A copy of the server's `serveClient` that uses
--   them, and a test server to try it.
-- 「Pub/Sub とブロッキング」のトレーニングの解答：UNSUBSCRIBE と、
-- いくつものキーの BLPOP。それを使う serveClient の写しと、
-- 試すためのテスト用のサーバー
module PubSubAnswers
  ( listenTo
  , blockingPopAny
  , serveClient2
  , withServer2
  ) where

import Control.Concurrent (newMVar, takeMVar, threadWaitRead, withMVar)
import Control.Concurrent.Async (link, race, withAsync)
import Control.Concurrent.STM
import Control.Exception (bracket, finally)
import Control.Monad (forM_, forever, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (toUpper)
import Data.List (unsnoc)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import MiniKv.Protocol (readRequest)
import MiniKv.Request
import MiniKv.Server
import MiniKv.Store (apply)
import MiniKv.Types
import MiniKv.Wire
-- ANCHOR_END: head

-- ANCHOR: listen
-- 👂 Step 2: listen to channels, and take requests while listening:
--    SUBSCRIBE more, UNSUBSCRIBE some (or all, with no names), PING.
--    When no channel is left, give back the bytes not read yet
--    (Just), to go back to plain requests. Nothing: the client left.
-- ステップ2：チャンネルを聞き、聞きながら要求も受け付ける：
-- SUBSCRIBE で足し、UNSUBSCRIBE で外す（名前なしなら全部）、PING。
-- 聞くチャンネルがなくなったら、まだ読んでいないバイトを返し（Just）、
-- ふつうのやり取りに戻る。Nothing は、クライアントが帰った
listenTo :: Env -> Socket -> [Text] -> ByteString -> IO (Maybe ByteString)
listenTo env sock first early = do
  mine  <- newTVarIO Set.empty
  inbox <- atomically (dupTChan (hub env))
  -- Two of us write to the socket: one at a time, or replies mix.
  -- ソケットに書くのは2人：1人ずつでないと、答えが混ざる
  turn <- newMVar ()
  let say w = withMVar turn (\() -> sendAll sock (render w))
      -- Our set and the counts change in the same transaction.
      -- 自分の一覧と人数は、同じトランザクションで変える
      change verb add c = do
        n <- atomically $ do
          here <- readTVar mine
          when (Set.member c here /= add) $
            modifyTVar' (listeners env)
              (Map.insertWith (+) c (if add then 1 else -1))
          let here' = (if add then Set.insert else Set.delete) c here
          writeTVar mine here'
          pure (Set.size here')
        say (WArray [bulk verb, bulk (encodeUtf8 c), WInt n])
      deliver = forever $ do
        (c, m) <- atomically (readTChan inbox)
        -- Ask "still listening?" holding the turn, so nothing is sent
        --   after the reply to the last UNSUBSCRIBE.
        -- 「まだ聞いている？」は、札を持って聞く。最後の UNSUBSCRIBE の
        -- 答えのあとに、何も送らないように
        withMVar turn $ \() -> do
          listening <- Set.member c <$> readTVarIO mine
          when listening $ sendAll sock $
            render (WArray [bulk "message", bulk (encodeUtf8 c), bulk' m])
      talk buffer = case readRequest buffer of
        NeedMore -> do
          chunk <- recv sock 4096
          if BS.null chunk then pure Nothing else talk (buffer <> chunk)
        Broken why -> do
          say (WError ("ERR protocol: " <> BC.pack why))
          pure Nothing
        Parsed [] rest -> talk rest
        Parsed (name : args) rest -> case (BC.map toUpper name, texts args) of
          (_, Nothing) -> say (WError "ERR not UTF-8") >> talk rest
          ("SUBSCRIBE", Just cs@(_ : _)) -> do
            mapM_ (change "subscribe" True) cs
            talk rest
          ("UNSUBSCRIBE", Just cs) -> do
            targets <- if null cs then Set.toList <$> readTVarIO mine
                                  else pure cs
            mapM_ (change "unsubscribe" False) targets
            left <- readTVarIO mine
            if Set.null left
              -- Keep the turn: the deliverer can send nothing more.
              -- 札は返さない：配る係は、もう何も送れない
              then takeMVar turn >> pure (Just rest)
              else talk rest
          ("PING", _) -> say (WArray [bulk "pong", bulk ""]) >> talk rest
          _ -> do
            say (WError "ERR only (UN)SUBSCRIBE and PING while listening")
            talk rest
      leaveAll = atomically $ do
        here <- readTVar mine
        forM_ here $ \c ->
          modifyTVar' (listeners env) (Map.adjust (subtract 1) c)
        writeTVar mine Set.empty
  let session = do
        mapM_ (change "subscribe" True) first
        either (const Nothing) id <$> race deliver (talk early)
  session `finally` leaveAll
  where
    bulk b  = WBulk (Just b)
    bulk' t = WBulk (Just (encodeUtf8 t))
    texts = traverse (either (const Nothing) Just . decodeUtf8')
-- ANCHOR_END: listen

-- ANCHOR: popAny
-- ⏳ Step 3: BLPOP on many keys. Wait until one of them has something
--    (the first in the order given), and answer the key and the value.
--    The rest is `blockingPop`, as it was.
-- ステップ3：いくつものキーの BLPOP。どれかに何か入るまで待ち
-- （並べた順に、最初のキー）、キーと値を答える。ほかは blockingPop のまま
blockingPopAny :: Env -> Socket -> [Key] -> Int
               -> IO (Maybe (Wire, ByteString))
blockingPopAny env sock keys seconds = do
  timeUp <- if seconds == 0 then newTVarIO False
                            else registerDelay (seconds * 1000000)
  let attempt early = do
        woke <- race waitForClient
                     (atomically ((Just <$> firstFilled)
                                  `orElse` (Nothing <$ waitFor timeUp)))
        case woke of
          Left () -> do
            chunk <- recv sock 4096
            if BS.null chunk
              then pure Nothing                 -- hung up / 切られた
              else attempt (early <> chunk)     -- keep it / 取っておく
          Right Nothing -> pure (Just (WBulk Nothing, early))
          Right (Just key) -> do
            reply <- runCommand env (LPop key)
            case reply of
              -- Someone was faster. / 先を越された
              WBulk Nothing -> attempt early
              WBulk value   ->
                pure (Just (WArray [WBulk (Just (encodeUtf8 key)),
                                    WBulk value], early))
              other         -> pure (Just (other, early))
  attempt BS.empty
  where
    waitForClient = withFdSocket sock (threadWaitRead . fromIntegral)
    -- STM's retry wakes up when any TVar we read changes: one key or
    --   ten, the waiting is the same.
    -- STM の retry は、読んだ TVar のどれかが変われば起きる：
    -- キーが1つでも 10 でも、待ち方は同じ
    firstFilled = do
      store <- readTVar (storeVar env)
      case filter (filled store) keys of
        key : _ -> pure key
        []      -> retry
    filled store key = case fst (apply 0 (LLen key) store) of
      Number 0 -> False
      _        -> True         -- an item, or an error / 何かある
    waitFor timeUp = readTVar timeUp >>= check
-- ANCHOR_END: popAny

-- 🧑‍💻 `serveClient`, with the two answers plugged in.
-- 2つの解答を差しこんだ serveClient
serveClient2 :: Env -> Socket -> IO ()
serveClient2 env sock = loop BS.empty
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
      Parsed words' rest -> case toRequest2 words' of
        Left message -> do
          sendAll sock (render (WError ("ERR " <> encodeUtf8 message)))
          answerAll rest
        Right (PopAny keys secs)     -> popAny keys secs rest
        Right (Old (BLPop key secs))   -> popAny [key] secs rest
        Right (Old (Subscribe channels)) -> do
          back <- listenTo env sock channels rest
          mapM_ answerAll back           -- Nothing: the client left
        Right (Old other) -> do
          reply <- case other of
            Publish c m   -> publish env c m
            Plain command -> runCommand env command
          sendAll sock (render reply)
          answerAll rest
    popAny keys secs rest = do
      waited <- blockingPopAny env sock keys secs
      case waited of
        Nothing             -> pure ()      -- the client left / 帰った
        Just (reply, early) -> do
          sendAll sock (render reply)
          answerAll (rest <> early)

-- 📮 The requests: the old ones, and BLPOP on many keys.
-- 要求：これまでのものと、いくつものキーの BLPOP
data Request2 = Old Request | PopAny [Key] Int

-- 🧩 BLPOP takes keys, then a timeout; the rest is `toRequest`.
-- BLPOP は、キーを並べて、最後に時間。ほかは toRequest のまま
toRequest2 :: [ByteString] -> Either Text Request2
toRequest2 words' = case words' of
  name : args | BC.map toUpper name == "BLPOP" -> case unsnoc args of
    Just (keys@(_ : _), t) -> PopAny <$> traverse utf8 keys <*> waitSeconds t
    _                      -> Left "BLPOP needs keys and a timeout"
  _ -> Old <$> toRequest words'
  where
    utf8 bytes = either (const (Left "not UTF-8")) Right (decodeUtf8' bytes)

-- 🧪 A test server that talks with `serveClient2`.
-- serveClient2 で話す、テスト用のサーバー
withServer2 :: (PortNumber -> IO a) -> IO a
withServer2 action = do
  (env, _) <- openEnv systemClock Nothing
  bracket (listenOn 0) close $ \server -> do
    port <- socketPort server
    withGuests $ \welcome -> do
      let acceptLoop2 = forever $ do
            (client, _) <- accept server
            welcome (serveClient2 env client `finally` close client)
      withAsync acceptLoop2 $ \acceptor -> do
        link acceptor
        action port
-- ANCHOR_END: all
