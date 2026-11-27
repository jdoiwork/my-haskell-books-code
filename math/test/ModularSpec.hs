module ModularSpec (spec) where

import Data.List (isPrefixOf, tails)
import Test.Hspec
import Test.QuickCheck

import Math.Modular
import ModularAnswers

spec :: Spec
spec = do
  -- ANCHOR: spec
  -- powMod は、ふつうに計算してから余りをとるのと同じ（法 1・指数 0 も）
  it "powMod agrees with (x ^ n) mod m" $
    forAll (choose (0, 1000)) $ \x -> forAll (choose (0, 60)) $ \n ->
      forAll (choose (1, 1000)) $ \m ->
        powMod x n m == (x ^ n) `mod` m
  -- 法が 1 なら、指数が 0 でも、答えは 0（1 mod 1）
  it "powMod with modulus 1 is 0" $
    powMod 262 0 1 `shouldBe` 0
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
  -- 並べ替えた "bcC" と "cCb" はハッシュがぶつかるが、文字も比べるので
  -- 見つかるのは本当の場所だけ
  it "findPattern checks the letters after a hash match" $ do
    stringHash "bcC" `shouldBe` stringHash "cCb"
    findPattern "bcC" "cCbbcC" `shouldBe` [3]
  -- ANCHOR_END: spec

  -- 端の値：組み合わせの範囲の外は 0、時計と gcd の符号
  it "the edges" $ do
    combinationsMod 5 (-1) `shouldBe` 0
    combinationsMod 5 6 `shouldBe` 0
    hourAfter 1 maxBound `shouldBe` 8
    let (g, x, y) = extendedGcd 6 (-4)
    (g, 6 * x + (-4) * y) `shouldBe` (2, 2)

  -- 「トレーニング」
  describe "answers" $ do
    it "100 days after a Monday is a Wednesday" $ do
      weekdayAfter Mon 100 `shouldBe` Wed
      weekdayAfter Tue maxBound `shouldBe` Tue
    it "978-0-306-40615-? ends with 7" $
      isbnCheckDigit [9, 7, 8, 0, 3, 0, 6, 4, 0, 6, 1, 5] `shouldBe` 7
