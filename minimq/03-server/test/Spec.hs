module Main (main) where

import Control.Concurrent.Async (forConcurrently)
import Data.Attoparsec.ByteString (IResult (..), feed, parse)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (Coercible, coerce)
import Data.List (sort)
import qualified Data.Text as T
import System.Timeout (timeout)
import Test.Hspec
import Test.QuickCheck

import MiniMq.Client
import MiniMq.Protocol
import MiniMq.Server
import MiniMq.Types
import MiniMq.Wire
import MqAnswers

-- 🎲 Random requests and responses. Bodies may hold spaces, newlines
--    and Japanese: the length in front keeps them apart.
-- でたらめな頼みごとと答え。本文には空白も改行も日本語も入る。
-- 先に書いた長さが、それを区切ってくれる
instance Arbitrary Request where
  arbitrary = oneof
    [ DeclareQueue    <$> name
    , DeclareExchange <$> name <*> elements [minBound ..]
    , Bind            <$> name <*> name <*> name
    , Publish         <$> name <*> letter
    , Consume         <$> name
    ]

instance Arbitrary Response where
  arbitrary = oneof
    [ pure Ok, RoutedTo . getNonNegative <$> arbitrary, pure NoRoute
    , Refused . T.pack <$> listOf (elements "no such queue:")
    , Deliver <$> letter
    ]

name :: Coercible T.Text n => Gen n
name = coerce . T.pack <$> listOf1 (elements "abcxyz.*#-")

letter :: Gen Message
letter = Message <$> name <*> (T.pack <$> listOf (elements "ab \n竜"))


bytesOf :: Wire a => a -> BS.ByteString
bytesOf = LBS.toStrict . toLazyByteString . render

-- 📖 Read from two chunks, as if the network cut the frame in two.
-- 2つのかたまりから読む（ネットワークが途中で切ったかのように）
readInTwo :: Wire a => Int -> BS.ByteString -> Maybe a
readInTwo at bytes =
  let (front, back) = BS.splitAt at bytes
  in  case feed (parse parser front) back of
        Done rest a | BS.null rest -> Just a
        _                          -> Nothing

main :: IO ()
main = hspec $ do
  -- ANCHOR: spec-wire
  describe "the wire" $ do
    -- 書いたものを読むと、元に戻る
    it "reads back every request it writes" $
      property $ \r -> readInTwo 0 (bytesOf r) === Just (r :: Request)
    it "reads back every response it writes" $
      property $ \r -> readInTwo 0 (bytesOf r) === Just (r :: Response)
    -- どこで2つに切れても、同じように読める
    it "reads a request cut anywhere in two" $
      property $ \r -> forAll (choose (0, BS.length (bytesOf r))) $ \at ->
        readInTwo at (bytesOf r) === Just (r :: Request)
  -- ANCHOR_END: spec-wire

  -- ANCHOR: spec-server
  describe "the server" $ do
    -- 送った手紙が、受け手に届く
    it "delivers a published letter to a consumer" $ withServer $ \port -> do
      withClient port $ \c -> do
        _ <- request c (DeclareQueue "jobs")
        _ <- request c (DeclareExchange "quests" Topic)
        _ <- request c (Bind "quests" "quest.#" "jobs")
        request c (Publish "quests" (Message "quest.dragon" "Red dragon"))
          `shouldReturn` RoutedTo 1
      withClient port $ \c -> do
        request c (Consume "jobs") `shouldReturn` Ok
        nextLetter c `shouldReturn` Message "quest.dragon" "Red dragon"
    -- ないものを頼むと、理由つきで断られる
    it "refuses names it does not know" $ withServer $ \port ->
      withClient port $ \c -> do
        request c (Consume "nobody")
          `shouldReturn` Refused "no such queue: nobody"
        request c (Publish "kitchen" (Message "x" "y"))
          `shouldReturn` Refused "no such exchange: kitchen"
    -- 3人の受け手が、1つのキューを分け合う。どの手紙も、ちょうど1回
    it "shares a queue among consumers, each letter exactly once" $
      withServer $ \port -> do
        setUp port
        withClient port $ \c ->
          mapM_ (request c . Publish "post" . letterNo) [1 .. 30]
        gathered <- forConcurrently [1 .. 3 :: Int] $ \_ ->
          withClient port $ \c -> request c (Consume "jobs") >> collect c
        sort (map body (concat gathered))
          `shouldBe` sort (map (body . letterNo) [1 .. 30])
    -- 受け取ってすぐ切った受け手の手紙は、もうどこにもない（この章の弱点）
    it "loses a letter whose consumer hangs up (the weak spot)" $
      withServer $ \port -> do
        setUp port
        withClient port $ \c -> request c (Publish "post" (letterNo 1))
        withClient port $ \c -> do
          _ <- request c (Consume "jobs")
          _ <- nextLetter c
          pure ()                        -- hang up without doing the job
        withClient port $ \c -> do
          _ <- request c (Consume "jobs")
          collect c `shouldReturn` []
  -- ANCHOR_END: spec-server

  -- 「トレーニング」
  describe "answers" $
    it "publishMany and takeLetters" $ withServer $ \port -> do
      setUp port
      withClient port $ \c ->
        publishMany c "post" (map letterNo [1, 2, 3])
          `shouldReturn` replicate 3 (RoutedTo 1)
      withClient port $ \c ->
        fmap (map body) (takeLetters c "jobs" 2) `shouldReturn` ["1", "2"]
  where
    setUp port = withClient port $ \c -> do
      _ <- request c (DeclareQueue "jobs")
      _ <- request c (DeclareExchange "post" Fanout)
      _ <- request c (Bind "post" "" "jobs")
      pure ()
    letterNo i = Message "job" (T.pack (show (i :: Int)))
    collect c = do
      next <- timeout 300000 (nextLetter c)
      maybe (pure []) (\l -> (l :) <$> collect c) next
