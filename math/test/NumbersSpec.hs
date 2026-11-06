module NumbersSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Math.Numbers
import NumbersAnswers

spec :: Spec
spec = do
  -- ANCHOR: spec
  -- 桁にして戻すと、元の数になる
  it "fromDigits undoes toDigits" $
    forAll (choose (2, 16)) $ \base ->
      forAll (choose (0, 10 ^ (12 :: Int))) $ \n ->
        fromDigits base (toDigits base n) == n
  -- addGold は、あふれないときは Integer の足し算と同じ
  it "addGold agrees with Integer when it says Just" $
    property $ \a b ->
      case addGold a b of
        Just c  -> toInteger c == toInteger a + toInteger b
        Nothing -> abs (toInteger a + toInteger b)
                     > toInteger (maxBound :: Int)
  -- maxBound に 1 を足すと Nothing
  it "addGold says Nothing at the edge" $
    addGold maxBound 1 `shouldBe` Nothing
  -- 立てたビットは、ほかのビットを変えない
  it "addStatus turns on only its own bit" $
    hasStatus sleep (addStatus poison noStatus) `shouldBe` False
  -- 105 セントに 10% の税で 116 セント（115.5 を切り上げ）
  it "withTax rounds half up" $
    map withTax [100, 105, 104] `shouldBe` [110, 116, 114]
  -- ANCHOR_END: spec

  -- ANCHOR: fastInvSqrt
  -- 魔法の数の答えは、本当の 1 / sqrt x と 0.2% もちがわない
  it "fastInvSqrt is within 0.2% of 1 / sqrt x" $
    forAll (choose (1.0e-3, 1.0e6)) $ \x ->
      abs (fastInvSqrt x * sqrt x - 1) < 0.002
  -- ANCHOR_END: fastInvSqrt

  -- 「トレーニング」
  describe "answers" $ do
    it "removeStatus cures only one status" $
      statusNames (removeStatus poison (addStatus silence
                     (addStatus poison noStatus)))
        `shouldBe` ["silence"]
    it "mulGold agrees with Integer when it says Just" $
      property $ \a b ->
        case mulGold a b of
          Just c  -> toInteger c == toInteger a * toInteger b
          Nothing -> True
    it "mulGold says Nothing for a huge product" $
      mulGold (maxBound `div` 2) 3 `shouldBe` Nothing
