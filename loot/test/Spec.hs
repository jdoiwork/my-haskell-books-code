-- ANCHOR: all
-- 🧪 All the tests of this chapter.
-- この章のテストすべて
module Main (main) where

import Test.Hspec

import qualified AnswersSpec
import qualified BagSpec
import qualified LootSpec
import qualified MonsterSpec
import qualified PartySpec

main :: IO ()
main = hspec $ do
  LootSpec.spec
  BagSpec.spec
  PartySpec.spec
  MonsterSpec.spec
  -- answers for the training / トレーニングの解答
  AnswersSpec.spec
-- ANCHOR_END: all
