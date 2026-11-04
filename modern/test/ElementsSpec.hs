{-# OPTIONS_GHC -Wno-orphans #-}
module ElementsSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Modern.Elements
import ModernAnswers

instance Arbitrary Element where
  arbitrary = elements [minBound .. maxBound]

spec :: Spec
spec = describe "Modern.Elements" $ do
  -- 相性表の例
  it "power" $ do
    power Fire Grass `shouldBe` 200
    power Fire Water `shouldBe` 50
    power Fire Fire `shouldBe` 100
  -- ANCHOR: notBothProp
  -- おたがいに強い組はない
  it "no two elements beat each other" $ property $ \a d ->
    notBoth a d
  -- ANCHOR_END: notBothProp
  -- 光と闇は、おたがいに強い
  it "light and dark" $ do
    beats6 Light Dark `shouldBe` True
    beats6 Dark Light `shouldBe` True
    beats6 (Basic Fire) (Basic Grass) `shouldBe` True
    beats6 Light (Basic Fire) `shouldBe` False
