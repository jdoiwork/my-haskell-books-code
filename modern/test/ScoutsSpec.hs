module ScoutsSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Either (isLeft)
import Data.IORef (newIORef, readIORef)
import Test.Hspec

import Modern.Scouts

spec :: Spec
spec = describe "Modern.Scouts" $ do
  -- ANCHOR: lostTest
  -- ばらばらの偵察は、親が戻ったあとも歩き続ける
  it "loose scouts keep walking after we return" $ do
    report <- newIORef []
    looseParty report
    before <- readIORef report
    threadDelay 200000
    after <- readIORef report
    (before, after) `shouldBe` ([], ["goblin", "slime"])
  -- 構造化された偵察は、失敗を知らせ、ほかの者を呼び戻す
  it "a structured party reports the trap and calls everyone back" $ do
    report <- newIORef []
    result <- structuredParty report
    threadDelay 200000
    names <- readIORef report
    (isLeft result, names) `shouldBe` (True, [])
  -- ANCHOR_END: lostTest
  -- 仕事が終われば、見張りも止まる
  it "the lookout stops with the work" $ do
    report <- newIORef []
    withLookout report (threadDelay 70000)
    seen <- length <$> readIORef report
    threadDelay 100000
    later <- length <$> readIORef report
    (seen >= 1, later == seen) `shouldBe` (True, True)
