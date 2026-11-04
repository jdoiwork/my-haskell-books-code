-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- 🏋️ Answers for the training.
-- トレーニングの解答
module AnswersSpec (spec) where

import Roster
import RosterSpec (withRoster)

import Control.Monad (replicateM_)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
  -- ANCHOR: step1
  around withRoster $
    describe "findAdventurer" $ do
      -- 「登録した冒険者は、名前で見つかる」
      it "finds an adventurer by name" $ \conn -> do
        addAdventurer conn (NewAdventurer "Lambda" "Mage" 3)
        found <- findAdventurer conn "Lambda"
        fmap (.job) found `shouldBe` Just "Mage"

      -- 「いない名前なら、Nothing」
      it "finds nobody for an unknown name" $ \conn -> do
        findAdventurer conn "Nobody" `shouldReturn` Nothing
  -- ANCHOR_END: step1

  -- ANCHOR: step2
  describe "levelUp" $
    -- 「n 回レベルを上げると、レベルが n 上がる」
    prop "raises the level by n after n times" $ \(Small n) ->
      ioProperty $ withRoster $ \conn -> do
        let times = abs n
        addAdventurer conn (NewAdventurer "Lambda" "Mage" 3)
        replicateM_ times (levelUp conn "Lambda")
        found <- findAdventurer conn "Lambda"
        pure (fmap (.level) found === Just (3 + times))
  -- ANCHOR_END: step2
-- ANCHOR_END: all
