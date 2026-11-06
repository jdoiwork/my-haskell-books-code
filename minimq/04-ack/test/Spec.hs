module Main (main) where

import Data.IORef
import qualified Data.Set as Set
import System.Timeout (timeout)
import Test.Hspec
import Test.QuickCheck

import MiniMq.Broker
import MiniMq.Client
import MiniMq.Examples
import MiniMq.Protocol
import MiniMq.Server
import MiniMq.Types
import MqAnswers

-- ANCHOR: model
-- 🎲 What can happen at the post office, in any order.
-- 伝令所で起こりうること（どんな順でも）
data Event
  = Send                       -- a new job / 新しい仕事の手紙
  | Take Int                   -- consumer i takes one / 受け手 i が1通取る
  | Answer Settlement Int      -- someone answers a held letter / 返事をする
  | Leave Int                  -- consumer i hangs up / 受け手 i が切る
  deriving Show

instance Arbitrary Event where
  arbitrary = frequency
    [ (3, pure Send), (3, Take <$> choose (0, 1))
    , (3, Answer <$> elements [minBound ..] <*> arbitrary)
    , (1, Leave <$> choose (0, 1)) ]

-- 🧮 Play the events, counting letters sent and letters finished.
-- 出来事を順に起こし、送った手紙と終わった手紙を数える
data World = World
  { broker  :: Broker
  , readers :: [ConsumerId]
  , held    :: [(Int, Tag)]
  , sent    :: Int
  , done    :: Int
  }

-- 🌅 The cursed post office (one letter sent), with two consumers.
-- 呪いの手紙が1通ある伝令所と、2人の受け手
morning :: World
morning = World b2 [c0, c1] [] 1 0
  where
    (c0, b1) = surely (subscribe "jobs" (surely cursedPost))
    (c1, b2) = surely (subscribe "jobs" b1)

play :: World -> Event -> World
play w event = case event of
  Send -> w { broker = snd (surely (publish "quests" job (broker w)))
            , sent   = sent w + 1 }
  Take i -> case surely (takeNext (who i) 2 "jobs" (broker w)) of
    (Just d,  b) -> w { broker = b, held = (i, tag d) : held w }
    (Nothing, _) -> w
  Answer how k | not (null (held w)) ->
    let (_, t) = held w !! (k `mod` length (held w))
    in  w { broker = surely (settle how t (broker w))
          , held   = filter ((/= t) . snd) (held w)
          , done   = done w + (if how == Ack then 1 else 0) }
  Answer _ _ -> w
  Leave i -> w { broker = abandon (who i) (broker w)
               , held   = filter ((/= i) . fst) (held w) }
  where
    job = Message "job" "Slay the slime"
    who i = readers w !! i

-- 📏 Every letter sent is somewhere: waiting, held, dead, or done.
-- 送った手紙は、どれもどこかにある：待っている、持たれている、
-- 配れなかった部屋にある、終わった
accounted :: World -> Int
accounted w =
  sum (map snd (waiting (broker w))) + unsettled (broker w) + done w
-- ANCHOR_END: model

main :: IO ()
main = hspec $ do
  -- ANCHOR: spec-core
  describe "the core" $ do
    -- 呪いの手紙は、3回で「配れなかった手紙の部屋」へ
    it "gives a letter up after three tries" $ do
      let (got, later) = surely (answerEach (replicate 4 Requeue)
                                            (surely cursedPost))
      map (fmap redelivered) got
        `shouldBe` [Just False, Just True, Just True, Nothing]
      waiting later `shouldBe` [("dead-letters", 1), ("jobs", 0)]
    -- 同じ番号札に2回返事をすると、断られる
    it "refuses a second answer to the same tag" $ do
      let (got, later) = surely (answerEach [Ack] (surely cursedPost))
      case got of
        [Just d] -> settle Ack (tag d) later
                      `shouldBe` Left (UnknownTag (tag d))
        _        -> expectationFailure "expected one letter"
    -- 何が起きても、手紙はなくならない
    it "never loses a letter, whatever happens" $
      forAll (listOf arbitrary) $ \events ->
        let end = foldl' play morning events
        in  accounted end === sent end
  -- ANCHOR_END: spec-core

  -- ANCHOR: spec-server
  describe "the server" $ do
    -- 返事をせずに切った受け手の手紙は、次の受け手に「again」で届く
    it "gives a dropped letter to the next consumer, marked again" $
      withServer $ \port -> do
        setUp port
        withClient port $ \c -> do
          _ <- request c (Consume "jobs" 1)
          _ <- nextDelivery c
          pure ()                              -- hang up, no answer
        withClient port $ \c -> do
          _ <- request c (Consume "jobs" 1)
          d <- nextDelivery c
          (message d, redelivered d)
            `shouldBe` (Message "job" "Open the cursed box", True)
    -- 1通ずつの受け手は、ACK するまで次をもらえない
    it "holds back the next letter until the first is answered" $
      withServer $ \port -> do
        setUp port
        _ <- withClient port $ \c ->
          request c (Publish "quests" (Message "job" "Second"))
        withClient port $ \c -> do
          _ <- request c (Consume "jobs" 1)
          first <- nextDelivery c
          timeout 200000 (nextDelivery c) `shouldReturn` Nothing
          answerWith Ack c first
          fmap (body . message) (nextDelivery c) `shouldReturn` "Second"
    -- 3回 REQUEUE された手紙は、dead-letters に届く
    it "moves a letter that keeps failing to dead-letters" $
      withServer $ \port -> do
        setUp port
        withClient port $ \c -> do
          _ <- request c (Consume "jobs" 1)
          let fail' = nextDelivery c >>= answerWith Requeue c
          fail' >> fail' >> fail'
        withClient port $ \c -> do
          _ <- request c (Consume "dead-letters" 1)
          fmap message (nextDelivery c)
            `shouldReturn` Message "job" "Open the cursed box"
  -- ANCHOR_END: spec-server

  -- 「トレーニング」
  describe "answers" $ do
    it "revive sends the dead letters back to work" $ do
      let (_, dead) = surely (answerEach (replicate 3 Requeue)
                                         (surely cursedPost))
      fmap waiting (revive dead)
        `shouldBe` Right [("dead-letters", 0), ("jobs", 1)]
    it "onlyOnce does a job once" $ do
      doneRef <- newIORef Set.empty
      count <- newIORef (0 :: Int)
      let d = Delivery (Tag 1) (Message "job" "Slay") False
          job _ = modifyIORef count (+ 1)
      onlyOnce doneRef job d
      onlyOnce doneRef job d { redelivered = True }
      readIORef count `shouldReturn` 1
  where
    setUp port = withClient port $ \c -> do
      let cursed = QueueSettings (Just "undeliverable") (Just 3)
      _ <- request c (DeclareExchange "quests" Direct)
      _ <- request c (DeclareExchange "undeliverable" Fanout)
      _ <- request c (DeclareQueue "jobs" cursed)
      _ <- request c (DeclareQueue "dead-letters" plainQueue)
      _ <- request c (Bind "quests" "job" "jobs")
      _ <- request c (Bind "undeliverable" "" "dead-letters")
      _ <- request c (Publish "quests" (Message "job" "Open the cursed box"))
      pure ()
