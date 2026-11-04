module Main (main) where

import Test.Hspec

import qualified DivideSpec
import qualified DpSpec
import qualified GraphsSpec
import qualified GreedySpec
import qualified PurelySpec
import qualified SchemesSpec
import qualified SearchSpec
import qualified StringsSpec
import qualified ThinkingSpec

main :: IO ()
main = hspec $ do
  ThinkingSpec.spec
  SearchSpec.spec
  DivideSpec.spec
  DpSpec.spec
  GreedySpec.spec
  GraphsSpec.spec
  StringsSpec.spec
  SchemesSpec.spec
  PurelySpec.spec
