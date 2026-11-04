-- ANCHOR: all
-- 🧪 All the tests of this chapter.
-- この章のテストすべて
module Main (main) where

import Test.Hspec

import qualified AnswersSpec
import qualified FirstSpec
import qualified ModelSpec
import qualified RosterSpec

main :: IO ()
main = hspec $ do
  FirstSpec.spec
  RosterSpec.spec
  ModelSpec.spec
  -- answers for the training / トレーニングの解答
  AnswersSpec.spec
-- ANCHOR_END: all
