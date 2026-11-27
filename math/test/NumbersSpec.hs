module NumbersSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Math.Numbers
import NumbersAnswers

-- ANCHOR: inIntRange
-- 📏 The answer an Int can hold: Just inside the range, Nothing outside.
--    The exact answer comes from Integer, which never overflows.
-- Int に入る答え：範囲の中なら Just、外なら Nothing。
-- 正しい答えは、あふれない Integer で計算しておく
inIntRange :: Integer -> Maybe Int
inIntRange exact
  | exact > toInteger (maxBound :: Int) = Nothing
  | exact < toInteger (minBound :: Int) = Nothing
  | otherwise                           = Just (fromInteger exact)
-- ANCHOR_END: inIntRange

spec :: Spec
spec = do
  -- ANCHOR: spec
  -- 桁にして戻すと、元の数になる
  it "fromDigits undoes toDigits" $
    forAll (choose (2, 16)) $ \base ->
      forAll (choose (0, 10 ^ (12 :: Int))) $ \n ->
        fromDigits base (toDigits base n) == n
  -- addGold は、Integer で足した答えが Int に入れば Just、
  -- 入らなければ Nothing
  it "addGold agrees with Integer" $
    property $ \a b ->
      addGold a b === inIntRange (toInteger a + toInteger b)
  -- 端の上と下
  it "addGold says Nothing just past the edges" $ do
    addGold maxBound 1 `shouldBe` Nothing
    addGold minBound (-1) `shouldBe` Nothing
    addGold minBound 0 `shouldBe` Just minBound
  -- 立てたビットだけが変わり、ほかのビットはそのまま
  it "addStatus turns on only its own bit" $
    and [ hasStatus w (addStatus which s) == (w == which || hasStatus w s)
        | which <- [poison, sleep, silence]
        , s     <- [0 .. 7]
        , w     <- [poison, sleep, silence] ]
  -- 105 セントに 10% の税で 116 セント（115.5 を切り上げ）。
  -- 大きな値段でも、途中の掛け算があふれない
  it "withTax rounds half up" $ do
    map withTax [100, 105, 104] `shouldBe` [110, 116, 114]
    withTax 83848836698679781 `shouldBe` 92233720368547759
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
    it "mulGold agrees with Integer" $
      property $ \a b ->
        mulGold a b === inIntRange (toInteger a * toInteger b)
    it "mulGold says Nothing for a huge product" $ do
      mulGold (maxBound `div` 2) 3 `shouldBe` Nothing
      mulGold 2 3 `shouldBe` Just 6
      mulGold minBound (-1) `shouldBe` Nothing
