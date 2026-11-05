module Main (main) where

import Test.Hspec

import qualified BoardSpec
import qualified CrdtSpec
import qualified EffectsSpec
import qualified ElementsSpec
import qualified McpSpec
import qualified OutcomeSpec
import qualified QuestFilesSpec
import qualified ScoutsSpec

main :: IO ()
main = hspec $ do
  ElementsSpec.spec
  OutcomeSpec.spec
  EffectsSpec.spec
  ScoutsSpec.spec
  CrdtSpec.spec
  QuestFilesSpec.spec
  McpSpec.spec
  BoardSpec.spec
