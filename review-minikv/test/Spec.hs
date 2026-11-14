module Main (main) where

-- ANCHOR: imports
import Data.IORef
import Test.Hspec
import Test.QuickCheck

import MiniKv.Client (withConnection)
import MiniKv.Server (withServerAt)
import MiniKv.Typed (run, seconds)
import qualified MiniKv.Typed as Typed
import Review.Gate

import GateProps
-- ANCHOR_END: imports

-- ANCHOR: examples
main :: IO ()
main = hspec $ do
  describe "hit" $ do
    -- 1分に3回まで。4回目は止められる
    it "lets three knocks in, and stops the fourth" $
      allowAll rule "lambda" [0, 1000, 2000, 3000]
        `shouldBe` [True, True, True, False]
    -- 60 秒たてば、また通れる
    it "opens again after the window" $
      allowAll rule "lambda" [0, 1000, 2000, 3000, 60000]
        `shouldBe` [True, True, True, False, True]
    -- 守れない決まり（0 回まで）では、だれも通さない
    it "lets nobody in under a rule it cannot keep" $
      or (allowAll (Rule 0 (secs 60)) "lambda" [0, 1, 60000])
        `shouldBe` False
    -- 0 秒の窓や、あふれる秒数の窓は、そもそも作れない
    it "cannot even make a window of 0 seconds, or of too many" $
      map seconds [0, -1, maxBound] `shouldBe` [Nothing, Nothing, Nothing]
  -- ANCHOR_END: examples
  -- ANCHOR: properties
  describe "properties" $ do
    -- 下書きの性質は、破られる（境目で、まとめて通れてしまう）
    it "the draft property fails" $
      property (expectFailure prop_slidingDraft)
    -- どの 60 秒でも、上限の 2 倍まで
    it "at most twice the limit in any window" $
      property prop_slidingTwice
    -- 最初の 3 回は通れる
    it "the first ones pass" $
      property prop_firstOnesPass
  -- ANCHOR_END: properties
  -- ANCHOR: network
  describe "allow (over the network)" $ do
    -- 時計を進めて、待たずに確かめる
    it "uses the server's clock, so the test never waits" $ do
      clock <- newIORef 0
      withServerAt (readIORef clock) $ \port ->
        withConnection port $ \conn -> do
          let gate = Rule { limit = 2, windowSeconds = secs 60 }
          a <- traverse (const (allow conn gate "python")) [1 .. 3 :: Int]
          writeIORef clock 61000
          b <- allow conn gate "python"
          (a, b) `shouldBe` (map Right [True, True, False], Right True)
    -- 守れない決まり（0回）では、通さず、数えもしない
    it "lets nobody in under a rule it cannot keep" $
      withServerAt (pure 0) $ \port ->
        withConnection port $ \conn -> do
          let bad = Rule { limit = 0, windowSeconds = secs 60 }
          answer <- allow conn bad "go"
          left <- run conn (Typed.Get (counterKey "go"))
          (answer, left) `shouldBe` (Right False, Right Nothing)
  -- ANCHOR_END: network
  -- ANCHOR: mendSpec
  describe "allowMending (step 4)" $ do
    -- INCR だけして落ちたあとでも、期限を付け直すので、窓が終われば通れる
    it "mends a count that was left without a deadline" $ do
      clock <- newIORef 0
      withServerAt (readIORef clock) $ \port ->
        withConnection port $ \conn -> do
          let gate = Rule { limit = 2, windowSeconds = secs 60 }
          -- The client fell right after INCR: a count, and no deadline.
          -- INCR の直後にクライアントが落ちた：数だけあって、期限がない
          _ <- run conn (Typed.Incr (counterKey "python"))
          let knock = allowMending conn gate "python"
          a <- traverse (const knock) [1, 2 :: Int]
          writeIORef clock 61000
          b <- allowMending conn gate "python"
          (a, b) `shouldBe` (map Right [True, False], Right True)
    -- 直さない allow だと、時計を進めても、閉じたまま
    it "the plain allow stays shut for ever in the same scene" $ do
      clock <- newIORef 0
      withServerAt (readIORef clock) $ \port ->
        withConnection port $ \conn -> do
          let gate = Rule { limit = 2, windowSeconds = secs 60 }
          _ <- run conn (Typed.Incr (counterKey "python"))
          a <- traverse (const (allow conn gate "python")) [1, 2 :: Int]
          writeIORef clock 61000
          b <- allow conn gate "python"
          (a, b) `shouldBe` (map Right [True, False], Right False)
    -- 直す門番も、守れない決まりでは、通さず、数えもしない
    it "lets nobody in under a rule it cannot keep, either" $
      withServerAt (pure 0) $ \port ->
        withConnection port $ \conn -> do
          let bad = Rule { limit = 0, windowSeconds = secs 60 }
          answer <- allowMending conn bad "go"
          left <- run conn (Typed.Get (counterKey "go"))
          (answer, left) `shouldBe` (Right False, Right Nothing)
  -- ANCHOR_END: mendSpec
