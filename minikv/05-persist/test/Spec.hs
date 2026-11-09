module Main (main) where

-- ANCHOR: imports
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (cancel, forConcurrently_, withAsync)
import Control.Exception (IOException, try)
import Data.Either (isLeft)
import Data.IORef
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Control.Monad (forever, replicateM_)
import Test.Hspec
import Test.QuickCheck hiding (replay)

import KvAnswers
import MiniKv.Client
import MiniKv.Persist
import MiniKv.Protocol
import MiniKv.Server
import MiniKv.Store
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

-- ANCHOR: timed
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
-- ANCHOR_END: timed

-- ANCHOR: genCommand
-- 🎲 Random commands on a few keys, so that kinds collide often.
-- 少ないキーへの、でたらめなコマンド（種類がよくぶつかるように）
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
-- ANCHOR_END: genCommand

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
  -- ANCHOR_END: persistSpec

  -- 「トレーニング」
  describe "answers" $ do
    it "getSet returns the old value" $
      fst (getSet 0 "k" "new" (snd (apply 0 (Set "k" "old") emptyStore)))
        `shouldBe` Bulk "old"
    it "kindCounts counts each kind" $
      kindCounts (snd (applyAll 0 [Set "a" "1", Set "b" "2", SAdd "s" ["x"]]
                                emptyStore))
        `shouldBe` [(KString, 2), (KSet, 1)]
  where
    emptyCollection v = case v of
      List l    -> null l
      Members s -> null s
      Hash h    -> null h
      Str _     -> False
