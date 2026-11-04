module Main (main) where

import Test.Hspec

import qualified AlgebraSpec
import qualified CalculusSpec
import qualified CountingSpec
import qualified GraphsSpec
import qualified GrowthSpec
import qualified InductionSpec
import qualified LogicSpec
import qualified ModularSpec
import qualified NumbersSpec
import qualified StatsSpec
import qualified VectorsSpec

main :: IO ()
main = hspec $ do
  describe "Math.Numbers" NumbersSpec.spec
  describe "Math.Logic" LogicSpec.spec
  describe "Math.Induction" InductionSpec.spec
  describe "Math.Growth" GrowthSpec.spec
  describe "Math.Counting" CountingSpec.spec
  describe "Math.Stats" StatsSpec.spec
  describe "Math.Modular" ModularSpec.spec
  describe "Math.Graphs" GraphsSpec.spec
  describe "Math.Vectors" VectorsSpec.spec
  describe "Math.Calculus" CalculusSpec.spec
  describe "Math.Algebra" AlgebraSpec.spec
