module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Control.Concurrent.Async (forConcurrently_, withAsync)
import Control.Concurrent.STM (atomically, check, newTVarIO, readTVar)
import Data.IORef
import Control.Monad (replicateM_)
import System.Timeout (timeout)
import Test.Hspec
import Test.QuickCheck

import KvAnswers
import MiniKv.Client
import MiniKv.Protocol
import MiniKv.Server
import MiniKv.Store
import MiniKv.Types
import MiniKv.Wire

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
-- ANCHOR: gens
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
  ]
  where
    key   = elements ["a", "b", "c"]
    -- the edge of Int, so that INCR overflows now and then
    -- Int の端の数も。INCR が、ときどきあふれるように
    text  = elements
      ["x", "y", "z", "1", "2", T.pack (show (maxBound :: Int))]
    small = choose (-3, 3)
-- ANCHOR_END: gens

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
    -- Int の端を越える INCR は、Overflow のエラーで、ストアは変わらない
    it "INCR at maxBound is an Overflow error" $ do
      let big             = T.pack (show (maxBound :: Int))
          (_, before)     = applyAll 0 [Set "n" big] emptyStore
          (reply, after)  = apply 0 (Incr "n") before
      reply `shouldBe` Failed (Overflow "n")
      after `shouldBe` before
    -- Int に入らない数や、16進の形は、数ではない
    it "INCR refuses numbers too big for Int, and 0x10" $
      fst (applyAll 0 [ Set "a" "9223372036854775808", Incr "a"
                      , Set "b" "0x10", Incr "b" ] emptyStore)
        `shouldBe` [ Ok, Failed (NotAnInteger "a")
                   , Ok, Failed (NotAnInteger "b") ]
    -- 同じキーを2回渡しても、消えたのは1つ
    it "DEL counts a key given twice once" $
      fst (applyAll 0 [Set "a" "1", Del ["a", "a"]] emptyStore)
        `shouldBe` [Ok, Number 1]
    -- どんなコマンドの列のあとも、空の入れ物は残らない
    it "never keeps an empty list, set or hash" $
      forAll (listOf genCommand) $ \commands ->
        let (_, s) = applyAll 0 commands emptyStore
        in  all (not . emptyCollection) (storedValues s)
    -- ANCHOR: failedSpec
    -- エラーを答えたコマンドは、見えるものを変えない（時刻が来たキーを
    -- 忘れることはある。だから、掃除したあとのストアでくらべる）
    it "a failed command changes nothing we can see" $
      forAll (listOf genCommand) $ \commands ->
        forAll genCommand $ \command ->
          let (_, s)       = applyAll 0 commands emptyStore
              (reply, s')  = apply 0 command s
          in  case reply of
                Failed _ -> snd (sweep 0 s') == snd (sweep 0 s)
                _        -> True
    -- ANCHOR_END: failedSpec
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
    -- 1 MB を超える要求は、一度に届いても、途中まででも断る
    it "refuses a request over 1 MB, however it arrives" $ do
      let value   = BC.replicate 400000 'v'
          request = render (WArray (map (WBulk . Just)
                      ["HSET", "k", value, value, value]))
      readRequest request `shouldBe` Broken "request too large"
      readRequest (BC.take 1100000 request)
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
    it "keeps an error on one line, whatever the key" $ do
      let reply = Failed (WrongType "a\r\n+OK" KList KString)
      parseWire (render (replyWire reply)) `shouldBe`
        Parsed (WError "WRONGTYPE a  +OK holds a string, not a list") ""
      -- toCommand のエラーも1行（知らないコマンドと、数でない引数）
      toCommand ["FOO\r\n+OK"] `shouldSatisfy` oneLineError
      toCommand ["INCRBY", "gold", "x\r\n+OK"] `shouldSatisfy` oneLineError
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
    -- TTL の残りは、秒に切り上げる（残り 0.999 秒でも 1）
    it "TTL rounds the time left up to whole seconds" $ do
      let (_, s) = applyAll 0 [Set "x" "1", Expire "x" 30] emptyStore
      fst (apply 3000 (Ttl "x") s) `shouldBe` Number 27
      fst (apply 29001 (Ttl "x") s) `shouldBe` Number 1
    -- PERSIST は期限を消す。2回目は、消す期限がないので 0
    it "PERSIST takes the deadline away" $ do
      let commands = [Set "k" "v", Expire "k" 5, Persist "k", Persist "k"]
          (replies, s) = applyAll 0 commands emptyStore
      replies `shouldBe` [Ok, Number 1, Number 1, Number 0]
      fst (apply 60000 (Get "k") s) `shouldBe` Bulk "v"
    -- 期限が Int の輪を回る EXPIRE は断り、キーはそのまま
    it "refuses an EXPIRE whose deadline would go round" $
      let now      = 1794000000000
          commands =
            [ Set "k" "v"
            , Expire "k" (maxBound `div` 1000 + 1)   -- × 1000 goes round
            , Expire "k" (maxBound `div` 1000)       -- now + goes round
            , Expire "k" minBound
            , Ttl "k" ]
      in  fst (applyAll now commands emptyStore)
            `shouldBe` [Ok] ++ replicate 3 (Failed (BadExpire "k"))
                         ++ [Number (-1)]
    -- 掃除係は、だれも触らないキーも忘れる（GET で先に忘れさせない）
    it "the sweeper forgets keys that nobody touches" $ do
      let commands = [Set "a" "1", Expire "a" 1, Set "b" "2"]
      var <- newTVarIO (snd (applyAll 0 commands emptyStore))
      swept <- withAsync (sweeper (pure 5000) var) $ \_ ->
        timeout 2000000 $ atomically $ do
          store <- readTVar var
          check (storedKeys store == ["b"])
      swept `shouldBe` Just ()
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

-- ✂️ An error that stays on one line (no \r, no \n).
-- 1行のままのエラー（\r も \n もない）
oneLineError :: Either T.Text Command -> Bool
oneLineError (Left message) = T.all (`notElem` ("\r\n" :: String)) message
oneLineError (Right _)      = False
