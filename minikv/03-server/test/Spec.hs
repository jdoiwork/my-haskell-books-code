module Main (main) where

-- ANCHOR: imports
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Monad (forever, replicateM, replicateM_)
import Network.Socket (close)
import Test.Hspec
import Test.QuickCheck

import KvAnswers
import ServerAnswers
import MiniKv.Client
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
            (_, s) = apply (Set key val) emptyStore
        in  fst (apply (Get key) s) == Bulk val
    -- LPUSH は先頭に積むので、逆順に並ぶ
    it "LPUSH puts items at the front" $
      fst (applyAll [LPush "l" ["a", "b", "c"], LRange "l" 0 (-1)] emptyStore)
        `shouldBe` [Number 3, Many ["c", "b", "a"]]
    -- 種類のちがう値に使うと、WrongType のエラー
    it "a list command on a string is a WrongType error" $
      fst (applyAll [Set "k" "v", LPush "k" ["x"]] emptyStore)
        `shouldBe` [Ok, Failed (WrongType "k" KList KString)]
    -- Int の端を越える INCR は、Overflow のエラー
    it "INCR at maxBound is an Overflow error" $
      fst (applyAll [Set "n" (T.pack (show (maxBound :: Int))), Incr "n"]
                    emptyStore)
        `shouldBe` [Ok, Failed (Overflow "n")]
    -- Int に入らない数や、16進の形は、数ではない
    it "INCR refuses numbers too big for Int, and 0x10" $
      fst (applyAll [ Set "a" "9223372036854775808", Incr "a"
                    , Set "b" "0x10", Incr "b" ] emptyStore)
        `shouldBe` [ Ok, Failed (NotAnInteger "a")
                   , Ok, Failed (NotAnInteger "b") ]
    -- 同じキーを2回渡しても、消えたのは1つ
    it "DEL counts a key given twice once" $
      fst (applyAll [Set "a" "1", Del ["a", "a"]] emptyStore)
        `shouldBe` [Ok, Number 1]
    -- どんなコマンドの列のあとも、空の入れ物は残らない
    it "never keeps an empty list, set or hash" $
      forAll (listOf genCommand) $ \commands ->
        let (_, s) = applyAll commands emptyStore
        in  all (not . emptyCollection) (storedValues s)
    -- エラーを答えたコマンドは、ストアを変えない
    it "a failed command leaves the store as it was" $
      forAll (listOf genCommand) $ \commands ->
        forAll genCommand $ \command ->
          let (_, s)       = applyAll commands emptyStore
              (reply, s')  = apply command s
          in  case reply of
                Failed _ -> s' == s
                _        -> True
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
    -- 長すぎる行は、一度に届いても断る
    it "refuses a too long line even when it arrives at once" $
      parseWire (render (WSimple (BC.replicate 70000 'a')))
        `shouldBe` Broken "line too long"
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
    -- nc で手で打った行（\n だけで終わる）も読める
    it "reads a plain line that ends in \\n alone" $
      readRequest "SET hero Lambda\nGET hero\n"
        `shouldBe` Parsed ["SET", "hero", "Lambda"] "GET hero\n"
    -- 1 MB たまっても終わらない要求は、待たずに断る
    it "refuses a request that keeps growing" $
      readRequest (BC.replicate 2000000 'a')
        `shouldBe` Broken "request too large"
    -- 数の引数は INCR と同じ読み方：0x10 や、Int に入らない数は断る
    it "refuses 0x10 and numbers too big for Int" $ do
      toCommand ["INCRBY", "gold", "0x10"]
        `shouldBe` Left "not an integer: 0x10"
      toCommand ["INCRBY", "gold", "9223372036854775808"]
        `shouldBe` Left "not an integer: 9223372036854775808"
    -- 種類ちがいのエラーは、WRONGTYPE で始まる
    it "renders WrongType as a WRONGTYPE error" $
      render (replyWire (Failed (WrongType "hero" KList KString)))
        `shouldBe` "-WRONGTYPE hero holds a string, not a list\r\n"
    -- キーに \r\n があっても、エラーは1行で、答えは1つ
    it "keeps an error on one line, whatever the key" $
      let reply = Failed (WrongType "a\r\n+OK" KList KString)
      in  parseWire (render (replyWire reply)) `shouldBe`
            Parsed (WError "WRONGTYPE a  +OK holds a string, not a list") ""
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

  -- 「トレーニング」
  describe "answers" $ do
    it "getSet returns the old value" $
      fst (getSet "k" "new" (snd (apply (Set "k" "old") emptyStore)))
        `shouldBe` Bulk "old"
    it "kindCounts counts each kind" $
      kindCounts (snd (applyAll [Set "a" "1", Set "b" "2", SAdd "s" ["x"]]
                                emptyStore))
        `shouldBe` [(KString, 2), (KSet, 1)]
    -- ステップ2：まとめて動かすと、半分だけの状態はだれにも見えない
    it "runCommands: nobody sees half of a purchase" $ do
      storeVar <- newTVarIO emptyStore
      _ <- runCommands storeVar [["SET", "gold", "90"]]
      let buy = runCommands storeVar [ ["INCRBY", "gold", "-30"]
                                     , ["RPUSH", "items", "potion"] ]
          seen = do
            s <- readTVarIO storeVar
            pure (fst (apply (Get "gold") s), fst (apply (LLen "items") s))
          whole (Bulk g, Number n) = read (T.unpack g) + 30 * n == (90 :: Int)
          whole _                  = False
      buyer  <- forkIO (forever buy)
      states <- replicateM 20000 seen
      killThread buyer
      all whole states `shouldBe` True
    -- ステップ3：つないで切ると、数が戻る
    it "acceptCounting: the count goes up and back down" $ do
      clients  <- newTVarIO 0
      storeVar <- newTVarIO emptyStore
      server   <- listenOn 0
      port     <- socketPort server
      loop     <- forkIO (acceptCounting clients storeVar server)
      let waitFor n = do
            c <- readTVarIO clients
            if c == n then pure () else threadDelay 1000 >> waitFor n
      withConnection port $ \_ -> waitFor (1 :: Int)
      waitFor 0
      killThread loop
      close server
  where
    emptyCollection v = case v of
      List l    -> null l
      Members s -> null s
      Hash h    -> null h
      Str _     -> False
