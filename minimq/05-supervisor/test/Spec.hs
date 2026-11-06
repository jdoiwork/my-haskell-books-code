module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race, withAsync)
import Control.Exception (throwIO, try)
import Control.Monad (forever, when)
import Data.IORef
import Data.List (mapAccumL, sort)
import System.Timeout (timeout)
import Test.Hspec
import Test.QuickCheck

import MiniMq.Client
import MiniMq.Protocol
import MiniMq.Server
import MiniMq.Supervisor
import MiniMq.Types
import MiniMq.Worker
import MqAnswers
import GHC.Clock (getMonotonicTime)

-- ANCHOR: decisions
-- 🧮 Ask `decide` again and again, as children fall at these times.
-- 子がこの時刻に倒れていったら、decide は何と言うか
decisions :: Intensity -> [Double] -> [Decision]
decisions limit times = snd (mapAccumL ask [] times)
  where
    ask history now =
      let (d, history') = decide OneForOne limit now ["a"] "a" history
      in  (history', d)
-- ANCHOR_END: decisions

-- 💥 A job that blows up. / 爆発する仕事
boom :: IO a
boom = throwIO (userError "boom")

main :: IO ()
main = hspec $ do
  -- ANCHOR: spec-decide
  describe "decide" $ do
    -- 誰を始め直すかは、やり方しだい
    it "restarts whom the strategy names" $ do
      let names = ["acceptor", "router", "janitor"]
          who how = fst (decide how (Intensity 3 10) 0 names "router" [])
      who OneForOne  `shouldBe` Restart ["router"]
      who OneForAll  `shouldBe` Restart names
      who RestForOne `shouldBe` Restart ["router", "janitor"]
    -- 10 秒に3回まで。4回目で、あきらめる
    it "gives up on the fourth fall within ten seconds" $
      decisions (Intensity 3 10) [0, 1, 2, 3]
        `shouldBe` [Restart ["a"], Restart ["a"], Restart ["a"], GiveUp]
    -- ゆっくり倒れるぶんには、何回でも始め直す
    it "never gives up when falls are far apart" $
      forAll (choose (1, 50)) $ \n ->
        let times = map (* 11) [0 .. fromIntegral (n :: Int)]
        in  all (/= GiveUp) (decisions (Intensity 3 10) times)
  -- ANCHOR_END: spec-decide

  -- ANCHOR: spec-supervise
  describe "supervise" $ do
    -- 2回倒れても、3回目に始めた子が働き続ける
    it "starts a fallen child again" $ do
      starts <- newIORef (0 :: Int)
      let flaky = do
            n <- atomicModifyIORef' starts (\k -> (k + 1, k + 1))
            when (n <= 2) boom
            forever (threadDelay 1000000)
          plan = Plan OneForOne (Intensity 3 10) [Child "flaky" flaky]
      _ <- race (supervise plan) (threadDelay 100000)
      readIORef starts `shouldReturn` 3
    -- 倒れ続ける子には、監督役があきらめる
    it "gives up on a child that keeps falling" $ do
      let plan = Plan OneForOne (Intensity 3 10) [Child "doomed" boom]
      try (supervise plan) >>= \case
        Left (TooManyRestarts name) -> name `shouldBe` "doomed"
        Right ()           -> expectationFailure "it should give up"
    -- あきらめた監督役は、上の監督役から見れば、倒れた子の1人
    it "makes a tree: a parent restarts a child supervisor" $ do
      starts <- newIORef (0 :: Int)
      let inner = supervise (Plan OneForOne (Intensity 1 10)
                                  [Child "doomed" (count starts >> boom)])
          outer = Plan OneForOne (Intensity 2 10) [Child "inner" inner]
      result <- try (supervise outer)
      case result of
        Left (TooManyRestarts name) -> name `shouldBe` "inner"
        Right ()           -> expectationFailure "it should give up"
      readIORef starts `shouldReturn` 6
  -- ANCHOR_END: spec-supervise

  -- ANCHOR: spec-worker
  describe "a worker under a supervisor" $
    -- 呪いの手紙で3回倒れても、ほかの手紙は全部終わり、
    -- 呪いの手紙は dead-letters へ
    it "finishes the good letters; the cursed one goes to dead-letters" $
      withServer $ \port -> do
        setUp port
        doneRef <- newIORef []
        let job letter
              | body letter == "Open the cursed box" = boom
              | otherwise = modifyIORef doneRef (body letter :)
            plan = Plan OneForOne (Intensity 5 10)
                        [Child "worker" (worker port "jobs" job)]
        withAsync (supervise plan) $ \_ -> do
          dead <- withClient port $ \c -> do
            _ <- request c (Consume "dead-letters" 1)
            timeout 3000000 (nextDelivery c)
          fmap (body . message) dead `shouldBe` Just "Open the cursed box"
          finished <- timeout 3000000 (waitUntil ((== 2) . length) doneRef)
          fmap sort finished `shouldBe` Just ["Find a herb", "Slay a slime"]
  -- ANCHOR_END: spec-worker

  -- 「トレーニング」
  describe "answers" $ do
    it "announced says the name at every start" $ do
      said <- newIORef []
      let child = announced (\t -> modifyIORef said (t :)) (Child "flaky" boom)
      _ <- try @TooManyRestarts
             (supervise (Plan OneForOne (Intensity 2 10) [child]))
      readIORef said `shouldReturn` replicate 3 "starting flaky"
    it "patient waits before starting again" $ do
      child <- patient 50000 (Child "flaky" boom)
      before <- getMonotonicTime
      _ <- try @TooManyRestarts
             (supervise (Plan OneForOne (Intensity 2 10) [child]))
      after' <- getMonotonicTime
      (after' - before > 0.1) `shouldBe` True
  where
    count ref = modifyIORef ref (+ 1)
    waitUntil ok ref = do
      now <- readIORef ref
      if ok now then pure now else threadDelay 10000 >> waitUntil ok ref
    setUp port = withClient port $ \c -> do
      let cursed = QueueSettings (Just "undeliverable") (Just 3)
          send' m = request c (Publish "quests" (Message "job" m))
      _ <- request c (DeclareExchange "quests" Direct)
      _ <- request c (DeclareExchange "undeliverable" Fanout)
      _ <- request c (DeclareQueue "jobs" cursed)
      _ <- request c (DeclareQueue "dead-letters" plainQueue)
      _ <- request c (Bind "quests" "job" "jobs")
      _ <- request c (Bind "undeliverable" "" "dead-letters")
      mapM_ send' ["Find a herb", "Open the cursed box", "Slay a slime"]
