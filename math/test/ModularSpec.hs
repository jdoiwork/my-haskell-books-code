module ModularSpec (spec) where

import Data.List (isPrefixOf, tails)
import Test.Hspec
import Test.QuickCheck

import Math.Modular
import ModularAnswers

spec :: Spec
spec = do
  -- ANCHOR: spec
  -- powMod は、ふつうに計算してから余りをとるのと同じ
  it "powMod agrees with (x ^ n) mod m" $
    forAll (choose (0, 1000)) $ \x -> forAll (choose (0, 60)) $ \n ->
      forAll (choose (1, 1000)) $ \m ->
        powMod x n m == (x ^ n) `mod` m
  -- 逆元を掛けると 1 に戻る
  it "a times its inverse is 1" $
    forAll (choose (1, 10 ^ (6 :: Int))) $ \a ->
      fmap (\i -> a * i `mod` 998244353) (inverseMod a 998244353)
        == Just 1
  -- Mod の足し算と掛け算は、分配法則を守る
  it "Mod keeps the distributive law" $
    property $ \a b c ->
      let x = fromInteger a :: Mod
          y = fromInteger b
          z = fromInteger c
      in  x * (y + z) == x * y + x * z
  -- RSA：暗号にして戻すと、元のメッセージ
  it "RSA decrypts what it encrypts" $
    forAll (choose (0, 3232)) $ \m -> rsaDecrypt (rsaEncrypt m) == m
  -- ハッシュで探しても、1文字ずつ比べて探しても、同じ場所が見つかる
  it "findPattern finds the same places as a plain search" $
    forAll (listOf (elements "ab")) $ \text ->
      forAll (listOf1 (elements "ab")) $ \pat ->
        let plain = [ i | (i, rest) <- zip [0 ..] (tails text)
                        , pat `isPrefixOf` rest ]
        in  findPattern pat text == plain
  -- ANCHOR_END: spec

  -- 「トレーニング」
  describe "answers" $ do
    it "100 days after a Monday is a Wednesday" $
      weekdayAfter Mon 100 `shouldBe` Wed
    it "978-0-306-40615-? ends with 7" $
      isbnCheckDigit [9, 7, 8, 0, 3, 0, 6, 4, 0, 6, 1, 5] `shouldBe` 7
