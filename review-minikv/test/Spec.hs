module Main (main) where

import Data.IORef
import Test.Hspec
import Test.QuickCheck

import MiniKv.Client (withConnection)
import MiniKv.Server (withServerAt)
import Review.Gate

import GateProps

main :: IO ()
main = hspec $ do
  -- ANCHOR: examples
  describe "hit" $ do
    -- 1分に3回まで。4回目は止められる
    it "lets three knocks in, and stops the fourth" $
      allowAll rule "lambda" [0, 1000, 2000, 3000]
        `shouldBe` [True, True, True, False]
    -- 60 秒たてば、また通れる
    it "opens again after the window" $
      allowAll rule "lambda" [0, 1000, 2000, 3000, 60000]
        `shouldBe` [True, True, True, False, True]
  -- ANCHOR_END: examples
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
  -- ANCHOR: network
  describe "allow (over the network)" $ do
    -- 時計を進めて、待たずに確かめる
    it "uses the server's clock, so the test never waits" $ do
      clock <- newIORef 0
      withServerAt (readIORef clock) $ \port ->
        withConnection port $ \conn -> do
          let gate = Rule { limit = 2, windowSeconds = 60 }
          a <- traverse (const (allow conn gate "python")) [1 .. 3 :: Int]
          writeIORef clock 61000
          b <- allow conn gate "python"
          (a, b) `shouldBe` (map Right [True, True, False], Right True)
  -- ANCHOR_END: network
