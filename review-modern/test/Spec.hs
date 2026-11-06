module Main (main) where

import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck

import Modern.Crdt (OrSet, members)
import Review.Board

-- 🎲 Changes about a few quests, from one device.
-- 少しの依頼についての変更。1台の端末から
changesFrom :: String -> Gen [Change]
changesFrom device = do
  steps <- listOf (elements [True, False])
  quests <- vectorOf (length steps) (elements ["slime", "herb", "dragon"])
  pure [ if isPost then Post (device, n) q else Finish q
       | (n, isPost, q) <- zip3 [0 ..] steps quests ]

start :: OrSet String
start = guildBoard

main :: IO ()
main = hspec $ do
  describe "Review.Board" $ do
    -- ANCHOR: convergence
    -- どちらの順に合わせても、同じ板になる
    it "both devices end with the same board" $
      forAll ((,) <$> changesFrom "lambda" <*> changesFrom "python") $
        \(a, b) ->
          let laptop = applyAll start a
              phone  = applyAll start b
          in members (laptop <> phone) === members (phone <> laptop)
    -- ANCHOR_END: convergence
    -- ANCHOR: naiveTest
    -- 下書きでは、ノート PC で終えた依頼が、合わせると戻ってくる
    it "the draft brings a finished quest back" $ do
      let first  = NaiveBoard (Set.fromList ["slime", "herb"])
          laptop = naiveApply first (Finish "slime")
          phone  = first
      naiveMerge laptop phone `shouldBe` first
    -- OR-Set なら、終えた依頼は戻らない
    it "the OR-Set keeps it finished" $ do
      let laptop = applyAll start [Finish "slime"]
          phone  = start
      members (laptop <> phone) `shouldBe` Set.fromList ["herb"]
    -- ANCHOR_END: naiveTest
    -- 2台が同時に作業して、合わせる
    it "syncDevices" $ do
      board <- syncDevices start
                 [Finish "slime"]
                 [Post ("python", 0) "dragon"]
      board `shouldBe` Set.fromList ["herb", "dragon"]
