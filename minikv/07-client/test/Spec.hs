module Main (main) where

-- ANCHOR: imports
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Control.Concurrent (modifyMVar_, newEmptyMVar, putMVar, takeMVar,
                           threadDelay, withMVar)
import Control.Concurrent.STM (readTVarIO)
import Control.Concurrent.Async (cancel, concurrently, forConcurrently_,
                                 poll, wait, withAsync)
import Control.Exception (IOException, try)
import Data.Either (isLeft)
import Data.IORef
import Data.Maybe (isNothing)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import System.IO (IOMode (..), hClose, openFile, openTempFile)
import System.Timeout (timeout)
import Control.Monad (forM_, forever, replicateM_)
import Test.Hspec
import Test.QuickCheck hiding (replay)

import KvAnswers
import PersistAnswers
import PubSubAnswers
import TypedAnswers
import MiniKv.Client
import MiniKv.Persist
import MiniKv.Protocol
import MiniKv.Server
import MiniKv.Store
import qualified MiniKv.Typed as Typed
import MiniKv.Typed (ClientError (..), ListKey (..), run)
import MiniKv.Types
import MiniKv.Wire
-- ANCHOR_END: imports

-- 🎲 Random wire values, any depth. / どんな深さでも、でたらめな回線の値
genWire :: Int -> Gen Wire
genWire depth = oneof (leaves ++ deeper)
  where
    deeper = [ WArray <$> few (genWire (depth - 1)) | depth > 0 ]
    leaves =
      [ WSimple <$> line, WError <$> line, WInt <$> arbitrary
      , WBulk <$> oneof [pure Nothing, Just . BS.pack <$> few arbitrary] ]
    line = BC.pack <$> few (elements "abc OK123")
    -- At most 4 of them, so the values stay small.
    -- 多くても4つ。値が大きくなりすぎないように
    few g = do
      n <- choose (0, 4)
      vectorOf n g

-- 🎲 Random commands on a few keys, so that kinds collide often.
-- 少ないキーへの、でたらめなコマンド（種類がよくぶつかるように）
-- 📝 Run commands, each at its own time (Nothing is the sweeper's
--    turn), and write the log a server would write.
-- コマンドを、それぞれの時刻に動かし（Nothing は掃除係の番）、
-- サーバーが書くはずのログを作る
runAndLog :: [(Millis, Maybe Command)] -> (Store, [Command])
runAndLog = go emptyStore []
  where
    go store logged [] = (store, concat (reverse logged))
    go store logged ((now, turn) : rest) =
      let (entries, store') = case turn of
            Just c  -> logOf (logStep now c store)
            Nothing -> logOf (sweepStep now store)
      in  go store' (entries : logged) rest
    logOf ((_, entries), store') = (entries, store')

-- 🎲 Commands at times that go forward, now and then the sweeper.
-- 進んでいく時刻に動かすコマンドと、ときどき掃除係
genTimed :: Gen [(Millis, Maybe Command)]
genTimed = do
  turns <- listOf $ (,) <$> choose (0, 3000)
                        <*> frequency [ (5, Just <$> genCommand)
                                      , (1, pure Nothing) ]
  let times = drop 1 (scanl (+) 1000 (map fst turns))
  pure (zip times (map snd turns))

genCommand :: Gen Command
genCommand = oneof
  [ Get <$> key, Set <$> key <*> text, Del <$> listOf1 key
  , Exists <$> key, Incr <$> key, IncrBy <$> key <*> small
  , LPush <$> key <*> listOf1 text, RPush <$> key <*> listOf1 text
  , LPop <$> key, RPop <$> key, LRange <$> key <*> small <*> small
  , LLen <$> key, SAdd <$> key <*> listOf1 text
  , SRem <$> key <*> listOf1 text, SMembers <$> key
  , SIsMember <$> key <*> text, HSet <$> key <*> text <*> text
  , HGet <$> key <*> text, HGetAll <$> key, TypeOf <$> key, pure Keys
  , Expire <$> key <*> small, Ttl <$> key, Persist <$> key
  , PExpireAt <$> key <*> choose (0, 9000)
  ]
  where
    key   = elements ["a", "b", "c"]
    text  = elements ["x", "y", "z", "1", "2"]
    small = choose (-3, 3)

main :: IO ()
main = hspec $ do
  -- ANCHOR: spec
  describe "apply" $ do
    -- SET したものは、GET で返ってくる
    it "GET returns what SET stored" $
      property $ \k v ->
        let key = T.pack k
            val = T.pack v
            (_, s) = apply 0 (Set key val) emptyStore
        in  fst (apply 0 (Get key) s) == Bulk val
    -- LPUSH は先頭に積むので、逆順に並ぶ
    it "LPUSH puts items at the front" $
      fst (applyAll 0 [LPush "l" ["a", "b", "c"], LRange "l" 0 (-1)]
                    emptyStore)
        `shouldBe` [Number 3, Many ["c", "b", "a"]]
    -- 種類のちがう値に使うと、WrongType のエラー
    it "a list command on a string is a WrongType error" $
      fst (applyAll 0 [Set "k" "v", LPush "k" ["x"]] emptyStore)
        `shouldBe` [Ok, Failed (WrongType "k" KList KString)]
    -- Int の端を越える INCR は、Overflow のエラー
    it "INCR at maxBound is an Overflow error" $
      fst (applyAll 0 [Set "n" (T.pack (show (maxBound :: Int))), Incr "n"]
                    emptyStore)
        `shouldBe` [Ok, Failed (Overflow "n")]
    -- どんなコマンドの列のあとも、空の入れ物は残らない
    it "never keeps an empty list, set or hash" $
      forAll (listOf genCommand) $ \commands ->
        let (_, s) = applyAll 0 commands emptyStore
        in  all (not . emptyCollection) (storedValues s)
  -- ANCHOR_END: spec

  -- ANCHOR: protocolSpec
  describe "wire" $ do
    -- 書いて読み直すと、元の値に戻り、後ろには何も残らない
    it "parses what it renders" $
      forAll (genWire 3) $ \w -> parseWire (render w) == Parsed w ""
    -- 途中までしか届いていなければ、続きを待つ
    it "waits for more when only a prefix has arrived" $
      forAll (genWire 3) $ \w ->
        let bytes = render w
        in  all (\k -> parseWire (BS.take k bytes) == NeedMore)
                [0 .. BS.length bytes - 1]
    -- 長すぎる文字列は、読む前に断る
    it "refuses a huge bulk string before reading it" $
      parseWire "$999999999\r\n" `shouldBe` Broken "bulk string too long"
  describe "protocol" $ do
    -- コマンドを単語にして、またコマンドに戻すと、元どおり
    it "toCommand undoes commandWords" $
      forAll genCommand $ \c -> toCommand (commandWords c) == Right c
    -- 配列の形の要求も、1行の要求も読める
    it "reads both kinds of requests" $ do
      readRequest (render (WArray [WBulk (Just "GET"), WBulk (Just "k")]))
        `shouldBe` Parsed ["GET", "k"] ""
      readRequest "set hero Lambda\r\nGET hero\r\n"
        `shouldBe` Parsed ["set", "hero", "Lambda"] "GET hero\r\n"
    -- 種類ちがいのエラーは、WRONGTYPE で始まる
    it "renders WrongType as a WRONGTYPE error" $
      render (replyWire (Failed (WrongType "hero" KList KString)))
        `shouldBe` "-WRONGTYPE hero holds a string, not a list\r\n"
  -- ANCHOR_END: protocolSpec

  -- ANCHOR: serverSpec
  describe "server" $ do
    -- つないで SET して GET すると、しまった値が返ってくる
    it "answers over TCP" $ withServer $ \port ->
      withConnection port $ \conn -> do
        r1 <- request conn ["SET", "hero", "Lambda"]
        r2 <- request conn ["GET", "hero"]
        (r1, r2) `shouldBe` (WSimple "OK", WBulk (Just "Lambda"))
    -- 知らないコマンドには、エラーで答える（つながりは切らない）
    it "answers unknown commands with an error" $ withServer $ \port ->
      withConnection port $ \conn -> do
        r1 <- request conn ["FLY", "away"]
        r2 <- request conn ["EXISTS", "away"]
        (r1, r2) `shouldBe`
          ( WError "ERR unknown command or wrong number of arguments: FLY"
          , WInt 0 )
    -- 10 人が同時に 200 回ずつ INCR しても、数えもれはない
    it "counts every INCR from many clients at once" $
      withServer $ \port -> do
        forConcurrently_ [1 .. 10 :: Int] $ \_ ->
          withConnection port $ \conn ->
            replicateM_ 200 (request conn ["INCR", "visits"])
        withConnection port $ \conn ->
          request conn ["GET", "visits"] `shouldReturn` WBulk (Just "2000")
  -- ANCHOR_END: serverSpec

  -- ANCHOR: expirySpec
  describe "expiry" $ do
    -- 期限の前は読めて、期限を過ぎると消えている
    it "forgets a key when its time has come" $ do
      let commands = [Set "session" "abc", Expire "session" 10]
          (_, s)   = applyAll 0 commands emptyStore
      fst (apply 9999 (Get "session") s) `shouldBe` Bulk "abc"
      fst (apply 10000 (Get "session") s) `shouldBe` Nil
    -- TTL は、ないキーで -2、期限なしで -1、ほかは残りの秒数
    it "TTL says -2, -1, or the seconds left" $
      let commands = [Ttl "x", Set "x" "1", Ttl "x", Expire "x" 30]
      in  fst (applyAll 0 commands emptyStore)
            `shouldBe` [Number (-2), Ok, Number (-1), Number 1]
    -- SET しなおすと、期限は消える
    it "SET clears the deadline" $ do
      let commands = [Set "k" "a", Expire "k" 5, Set "k" "b"]
          (_, s)   = applyAll 0 commands emptyStore
      fst (apply 60000 (Get "k") s) `shouldBe` Bulk "b"
    -- 掃除は、時刻 t に KEYS が忘れるのと同じキーを忘れ、その数を答える
    it "sweep forgets just the keys whose time has come" $
      checkCoverage $
        forAll (listOf genCommand) $ \commands ->
          forAll (choose (0, 9000)) $ \t ->
            let (_, s)  = applyAll 0 commands emptyStore
                (n, s') = sweep t s
                gone    = length (storedKeys s) - length (storedKeys s')
            in  cover 5 (n > 0) "some key was swept" $
                  (n, s') === (gone, snd (apply t Keys s))
    -- サーバーでも、手で進める時計で、待たずに確かめる
    it "expires keys on the server, with a clock moved by hand" $ do
      clock <- newIORef 0
      withServerAt (readIORef clock) $ \port ->
        withConnection port $ \conn -> do
          _ <- request conn ["SET", "session", "abc"]
          _ <- request conn ["EXPIRE", "session", "10"]
          writeIORef clock 11000          -- 11 seconds later / 11 秒後
          request conn ["GET", "session"] `shouldReturn` WBulk Nothing
  -- ANCHOR_END: expirySpec

  -- ANCHOR: persistSpec
  describe "persistence" $ do
    -- ログを読み戻すと、そのまま動かしたのと同じストアになる
    -- （時刻が進み、期限が切れ、掃除係も動く並びで）
    it "replaying the log gives the very same store" $
      forAll genTimed $ \turns ->
        let (direct, logged) = runAndLog turns
            bytes = foldMap encodeEntry logged
        in  replayedStore (replay bytes) === direct
    -- 途中で切れたログは、そろった件数だけ使う
    it "a log cut anywhere gives a prefix of the entries" $
      forAll genTimed $ \turns ->
        let (_, logged) = runAndLog turns
            sizes = map (BS.length . encodeEntry) logged
            bytes = foldMap encodeEntry logged
        in  forAll (choose (0, BS.length bytes)) $ \k ->
              let r        = replay (BS.take k bytes)
                  -- How many entries end within the first k bytes.
                  -- 先頭の k バイトの中で終わっている件数
                  complete = length (takeWhile (<= k) (scanl1 (+) sizes))
                  first    = take complete logged
              in  (entries r, replayedStore r)
                    === (complete, snd (applyAll 0 first emptyStore))
    -- スナップショットから作り直すと、まったく同じストア
    it "a rewritten log rebuilds the very same store" $
      forAll genTimed $ \turns ->
        let (store, _) = runAndLog turns
        in  replayedStore (replay (rewriteLog store)) === store
    -- サーバーを止めて、また立てても、データは残っている
    it "keeps the data across a restart" $ do
      dir <- getTemporaryDirectory
      let path = dir </> "minikv-test.log"
      writeFile path ""
      withServerLog systemClock (Just path) $ \port ->
        withConnection port $ \conn -> do
          _ <- request conn ["SET", "hero", "Lambda"]
          _ <- request conn ["RPUSH", "party", "Lambda", "Python"]
          pure ()
      reply <- withServerLog systemClock (Just path) $ \port ->
        withConnection port $ \conn ->
          request conn ["LRANGE", "party", "0", "-1"]
      removeFile path
      reply `shouldBe` WArray [WBulk (Just "Lambda"), WBulk (Just "Python")]
    -- 期限が切れたあとに書いた値も、立て直したあとに残っている
    it "keeps a value written after its key expired" $ do
      dir <- getTemporaryDirectory
      let path = dir </> "minikv-expired.log"
      writeFile path ""
      clock <- newIORef 1000
      withServerLog (readIORef clock) (Just path) $ \port ->
        withConnection port $ \conn -> do
          _ <- request conn ["SET", "k", "5"]
          _ <- request conn ["EXPIRE", "k", "2"]
          writeIORef clock 10000       -- k is gone / k はもう忘れた
          request conn ["INCR", "k"] `shouldReturn` WInt 1
      reply <- withServerLog (readIORef clock) (Just path) $ \port ->
        withConnection port $ \conn -> request conn ["GET", "k"]
      removeFile path
      reply `shouldBe` WBulk (Just "1")
    -- 途中で切れたログのあとに書いたものも、立て直したあとに読み戻せる
    it "keeps what was written after a log that was cut short" $ do
      dir <- getTemporaryDirectory
      let path = dir </> "minikv-cut.log"
          -- A SET, then a DEL cut short (the server stopped while
          --   writing it).
          -- SET と、途中で切れた DEL（書いている途中で、サーバーが止まった）
          cut  = encodeEntry (Set "hero" "Lambda") <> "*2\r\n$3\r\nDEL"
      BS.writeFile path cut
      withServerLog systemClock (Just path) $ \port ->
        withConnection port $ \conn ->
          request conn ["SET", "gold", "10"] `shouldReturn` WSimple "OK"
      r <- replay <$> BS.readFile path
      removeFile path
      (problem r, replayedStore r) `shouldBe`
        ( Nothing
        , snd (applyAll 0 [Set "hero" "Lambda", Set "gold" "10"] emptyStore) )
      -- The old log is kept aside. / 古いログは、写しを残してある
      BS.readFile (path <> ".broken") `shouldReturn` cut
      removeFile (path <> ".broken")
    -- サーバーが止まったら、つながっている客にも電話を切る
    it "hangs up on its clients when it stops" $ do
      started <- newEmptyMVar
      let server = withServer $ \port -> do
            putMVar started port
            forever (threadDelay 1000000)
      withAsync server $ \running -> do
        port <- takeMVar started
        withConnection port $ \conn -> do
          request conn ["SET", "hero", "Lambda"] `shouldReturn` WSimple "OK"
          cancel running
          -- Not answered by a guest left behind: told it is over, at once.
          -- 置いていかれた客に答えられず、すぐに切れたとわかる
          late <- timeout 5000000 $
            try @IOException (request conn ["GET", "hero"])
          fmap isLeft late `shouldBe` Just True
    -- ログに書けなくなったら、変更を断り、それからあとも断り続ける。
    -- 読むのは続けられ、断った変更は、ストアにもログにもない
    it "refuses changes once the log cannot be written" $ do
      dir <- getTemporaryDirectory
      (path, h) <- openTempFile dir "minikv-full.log"
      hClose h
      (env, _) <- openEnv systemClock (Just path)
      let misconf reply = case reply of
            WError e -> "MISCONF" `BS.isPrefixOf` e
            _        -> False
      runCommand env (Set "hero" "Lambda") `shouldReturn` WSimple "OK"
      -- The disk breaks: closing the handle makes every write fail.
      -- ディスクが壊れた：Handle を閉じると、書くたびに失敗する
      forM_ (journal env) $ \lock -> withMVar lock hClose
      runCommand env (Set "hero" "Python") >>= (`shouldSatisfy` misconf)
      -- The disk is back (a fresh handle), but changes stay refused.
      -- ディスクは戻った（新しい Handle）が、変更は断られたまま
      forM_ (journal env) $ \lock ->
        modifyMVar_ lock (\_ -> openFile path AppendMode)
      runCommand env (Set "gold" "10") >>= (`shouldSatisfy` misconf)
      -- Reads go on, and see none of the refused changes.
      -- 読むのは続けられ、断った変更は見えない
      runCommand env (Get "hero") `shouldReturn` WBulk (Just "Lambda")
      runCommand env (Get "gold") `shouldReturn` WBulk Nothing
      forM_ (journal env) $ \lock -> withMVar lock hClose
      -- The log has just the change that was answered OK.
      -- ログには、OK と答えた変更だけ
      r <- replay <$> BS.readFile path
      removeFile path
      (problem r, replayedStore r) `shouldBe`
        (Nothing, snd (applyAll 0 [Set "hero" "Lambda"] emptyStore))
  -- ANCHOR_END: persistSpec

  -- ANCHOR: pubsubSpec
  describe "blocking and pub/sub" $ do
    -- BLPOP は、ほかのクライアントが RPUSH するまで待つ
    it "BLPOP waits for another client's RPUSH" $ withServer $ \port ->
      withConnection port $ \waiter ->
        withAsync (request waiter ["BLPOP", "jobs", "5"]) $ \popping -> do
          threadDelay 200000                -- 0.2 s later / 0.2 秒後
          -- Still waiting: nothing has come yet. / まだ待っている
          isNothing <$> poll popping `shouldReturn` True
          _ <- withConnection port $ \c ->
            request c ["RPUSH", "jobs", "slay goblins"]
          wait popping `shouldReturn` WBulk (Just "slay goblins")
    -- 時間切れなら、何もない答え
    it "BLPOP gives nil when the time is up" $ withServer $ \port ->
      withConnection port $ \c ->
        request c ["BLPOP", "empty", "1"] `shouldReturn` WBulk Nothing
    -- 2人が待っていても、1つの仕事を受け取るのは1人だけ
    it "one item goes to exactly one of two waiting clients" $
      withServer $ \port -> do
        let waiter = withConnection port $ \c ->
              request c ["BLPOP", "jobs", "1"]
        ((a, b), _) <- concurrently (concurrently waiter waiter)
          (withConnection port $ \c -> do
             threadDelay 100000
             request c ["RPUSH", "jobs", "only one"])
        filter (/= WBulk Nothing) [a, b]
          `shouldBe` [WBulk (Just "only one")]
    -- 待っているあいだに切ったクライアントは、仕事を持っていかない
    it "a waiter who hangs up takes no item" $ withServer $ \port -> do
      -- 20 clients start a BLPOP with no time limit, and hang up
      --   0.1 s later.
      -- 20 人が、時間切れなしの BLPOP を始め、0.1 秒後に切る
      forConcurrently_ [1 .. 20 :: Int] $ \_ ->
        withConnection port $ \c ->
          timeout 100000 (request c ["BLPOP", "jobs", "0"])
      -- Give the server time to notice. / サーバーが気づくのを待つ
      threadDelay 200000
      withConnection port $ \c -> do
        _ <- request c ["RPUSH", "jobs", "slay goblins"]
        request c ["BLPOP", "jobs", "1"]
          `shouldReturn` WBulk (Just "slay goblins")
    -- 購読しているクライアントに、メッセージが届く
    it "delivers a published message to a subscriber" $
      withServer $ \port ->
        withConnection port $ \listener -> do
          joined <- request listener ["SUBSCRIBE", "news"]
          heard <- withConnection port $ \c ->
            request c ["PUBLISH", "news", "dragon spotted"]
          -- Not forever: a message that never comes fails in 5 s.
          -- いつまでも待たない：届かなければ 5 秒で失敗
          message <- timeout 5000000 (receive listener)
          (joined, heard, message) `shouldBe`
            ( WArray [WBulk (Just "subscribe"), WBulk (Just "news"), WInt 1]
            , WInt 1
            , Just (WArray [ WBulk (Just "message"), WBulk (Just "news")
                           , WBulk (Just "dragon spotted") ]) )
  -- ANCHOR_END: pubsubSpec
    -- 購読してすぐ切った人は、人数から外れる。（返事を送る途中で
    -- 切られるすき間は、このテストではまず起きない。bracket で
    -- 閉じたのは、読んで確かめた）
    it "takes subscribers who hang up at once off the count" $
      withServer $ \port -> do
        let channels = [ BC.pack ("ch" <> show i) | i <- [1 .. 50 :: Int] ]
        forConcurrently_ [1 .. 20 :: Int] $ \_ ->
          withConnection port $ \c -> request c ("SUBSCRIBE" : channels)
        withConnection port $ \c -> do
          -- Leaving takes a moment; ask again until nobody is counted.
          -- 外れるのには少し時間がかかるので、0 人になるまで聞き直す
          let settle :: Int -> IO Wire
              settle tries = do
                heard <- request c ["PUBLISH", "ch50", "anyone?"]
                if heard == WInt 0 || tries == 0
                  then pure heard
                  else threadDelay 10000 >> settle (tries - 1)
          settle 300 `shouldReturn` WInt 0

  -- ANCHOR: typedSpec
  describe "typed client" $ do
    -- 型付きのキーで、しまって、取り出す
    it "keeps the types of keys and answers" $ withServer $ \port ->
      withConnection port $ \conn -> do
        let gold  = Typed.Key "gold" :: Typed.Key Int
            hero  = Typed.Key "hero" :: Typed.Key T.Text
            party = ListKey "party" :: ListKey T.Text
        _ <- run conn (Typed.Set hero "Lambda")
        a <- run conn (Typed.Incr gold)
        b <- run conn (Typed.Incr gold)
        c <- run conn (Typed.Get hero)
        d <- run conn (Typed.RPush party ["Lambda", "Python"])
        e <- run conn (Typed.LRange party 0 (-1))
        (a, b, c, d, e) `shouldBe`
          (Right 1, Right 2, Right (Just "Lambda"), Right 2,
           Right ["Lambda", "Python"])
    -- 数のつもりのキーに、ほかの人が文字列を入れたら、読むときに気づく
    it "notices a value that does not decode" $ withServer $ \port ->
      withConnection port $ \conn -> do
        _ <- request conn ["SET", "level", "high"]
        let level = Typed.Key "level" :: Typed.Key Int
        answer <- run conn (Typed.Get level)
        answer `shouldBe` Left (NotDecodable "high")
    -- サーバーのエラーも、型付きで返ってくる
    it "passes on the server's errors" $ withServer $ \port ->
      withConnection port $ \conn -> do
        _ <- request conn ["RPUSH", "hero", "x"]
        answer <- run conn (Typed.Incr (Typed.Key "hero"))
        answer `shouldBe`
          Left (ServerSaid "WRONGTYPE hero holds a list, not a string")
    -- パイプラインでも、答えは要求の順に返ってくる
    it "pipelining keeps the order of the answers" $ withServer $ \port ->
      withConnection port $ \conn -> do
        answers <- requestMany conn (replicate 50 ["INCR", "n"])
        answers `shouldBe` map WInt [1 .. 50]
    -- トレーニングの解答：GET と SET で、ゴールドを足す
    it "addGold adds to a number that may be missing" $ withServer $ \port ->
      withConnection port $ \conn -> do
        let gold = Typed.Key "gold" :: Typed.Key Int
        a <- addGold conn gold 30
        b <- addGold conn gold 12
        (a, b) `shouldBe` (Right 30, Right 42)
  -- ANCHOR_END: typedSpec

  -- ANCHOR: shareSpec
  describe "sharing one connection" $ do
    -- 1本のつながりを 20 のスレッドで使っても、答えは自分のもの
    it "gives every thread its own answers" $ withServer $ \port ->
      withConnection port $ \conn -> do
        let key i = BC.pack ("k" <> show i)
        forM_ [1 .. 20 :: Int] $ \i ->
          request conn ["SET", key i, key i]
        forConcurrently_ [1 .. 20 :: Int] $ \i ->
          replicateM_ 50 $
            request conn ["GET", key i] `shouldReturn` WBulk (Just (key i))
    -- 途中で止めた要求の答えを、次の要求が読まない
    it "never reads the late reply of a request cut short" $
      withServer $ \port -> withConnection port $ \conn -> do
        _ <- request conn ["SET", "hero", "Lambda"]
        -- The server answers BLPOP (with nil) only after 1 s.
        -- サーバーが BLPOP に（何もないと）答えるのは、1 秒後
        cut <- timeout 100000 (request conn ["BLPOP", "jobs", "1"])
        cut `shouldBe` Nothing
        -- With the bug, this GET read BLPOP's late nil.
        -- まちがった作りでは、この GET が、BLPOP の遅れた答えを読んだ
        next <- try @IOException (request conn ["GET", "hero"])
        next `shouldSatisfy` isLeft
  -- ANCHOR_END: shareSpec

  -- 「トレーニング」
  describe "answers" $ do
    it "getSet returns the old value" $
      fst (getSet 0 "k" "new" (snd (apply 0 (Set "k" "old") emptyStore)))
        `shouldBe` Bulk "old"
    it "kindCounts counts each kind" $
      kindCounts (snd (applyAll 0 [Set "a" "1", Set "b" "2", SAdd "s" ["x"]]
                                emptyStore))
        `shouldBe` [(KString, 2), (KSet, 1)]
    -- ANCHOR: compactSpec
    -- ログを書き直すあいだに書いたものも、なくならない
    it "compactLog loses nothing written meanwhile" $ do
      dir <- getTemporaryDirectory
      (path, h) <- openTempFile dir "minikv-compact.log"
      hClose h
      (env, _) <- openEnv systemClock (Just path)
      let name i = "n" <> show (i :: Int)
          -- 10 writers add 1 to their own key, 200 times each.
          -- 10 人が、自分のキーに 1 を足す。200 回ずつ
          writers = forConcurrently_ [1 .. 10] $ \i ->
            replicateM_ 200 (runCommand env (Incr (T.pack (name i))))
          -- Meanwhile, the log is made short again and again, until
          --   the writers are done.
          -- そのあいだ、書く人が終わるまで、ログを何度も短くする
          compactor done n = do
            finished <- readIORef done
            if finished
              then pure n
              else compactLog env path >> compactor done (n + 1 :: Int)
      done <- newIORef False
      (_, times) <- concurrently (writers >> writeIORef done True)
                                 (compactor done 0)
      forM_ (journal env) $ \lock -> withMVar lock hClose
      r <- replay <$> BS.readFile path
      live <- readTVarIO (storeVar env)
      removeFile path
      -- The log gives the store back, all 200s.
      -- ログから、ストアがそのまま戻る。どれも 200
      times `shouldSatisfy` (> 0)
      (problem r, replayedStore r) `shouldBe` (Nothing, live)
      [ fst (apply 0 (Get (T.pack (name i))) live) | i <- [1 .. 10] ]
        `shouldBe` replicate 10 (Bulk "200")
    -- ANCHOR_END: compactSpec
    -- ANCHOR: pubsubAnswerSpec
    -- UNSUBSCRIBE で聞くのをやめ、全部やめたら、ふつうのやり取りに戻る
    it "UNSUBSCRIBE stops listening, then plain requests work" $
      withServer2 $ \port -> withConnection port $ \listener -> do
        let shout c m = withConnection port $ \other ->
              request other ["PUBLISH", c, m]
            bulk b = WBulk (Just b)
        _ <- request listener ["SUBSCRIBE", "news", "gossip"]
        _ <- receive listener              -- for gossip / gossip の分
        left <- request listener ["UNSUBSCRIBE", "news"]
        news <- shout "news" "dragon spotted"
        gossip <- shout "gossip" "a rumor"
        -- Not "dragon spotted": news is no longer heard.
        -- 「dragon spotted」ではない：news は、もう聞いていない
        heard <- timeout 5000000 (receive listener)
        bye <- request listener ["UNSUBSCRIBE"]
        set <- request listener ["SET", "hero", "Lambda"]
        nobody <- shout "gossip" "anyone?"
        (left, news, gossip, heard) `shouldBe`
          ( WArray [bulk "unsubscribe", bulk "news", WInt 1]
          , WInt 0
          , WInt 1
          , Just (WArray [bulk "message", bulk "gossip", bulk "a rumor"]) )
        (bye, set, nobody) `shouldBe`
          ( WArray [bulk "unsubscribe", bulk "gossip", WInt 0]
          , WSimple "OK"
          , WInt 0 )
    -- いくつものキーを見張り、どのキーから取り出したかも答える
    it "BLPOP on many keys answers the key and the value" $
      withServer2 $ \port -> withConnection port $ \c -> do
        let bulk b = WBulk (Just b)
            popper = withConnection port $ \w ->
              request w ["BLPOP", "jobs", "urgent", "5"]
        _ <- request c ["RPUSH", "jobs", "slay goblins"]
        _ <- request c ["RPUSH", "urgent", "dragon"]
        -- Both have items: the first key in the list wins.
        -- どちらにもある：並べた順で、最初のキー
        first <- request c ["BLPOP", "urgent", "jobs", "1"]
        _ <- request c ["LPOP", "jobs"]
        waited <- withAsync popper $ \popping -> do
          threadDelay 200000                -- 0.2 s later / 0.2 秒後
          isNothing <$> poll popping `shouldReturn` True
          _ <- request c ["RPUSH", "urgent", "wyvern"]
          wait popping
        none <- request c ["BLPOP", "jobs", "urgent", "1"]
        (first, waited, none) `shouldBe`
          ( WArray [bulk "urgent", bulk "dragon"]
          , WArray [bulk "urgent", bulk "wyvern"]
          , WBulk Nothing )
    -- ANCHOR_END: pubsubAnswerSpec
  where
    emptyCollection v = case v of
      List l    -> null l
      Members s -> null s
      Hash h    -> null h
      Str _     -> False
