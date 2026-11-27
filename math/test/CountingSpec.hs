module CountingSpec (spec) where

import Data.Ratio ((%))
import Test.Hspec
import Test.QuickCheck

import CountingAnswers
import Math.Counting

spec :: Spec
spec = do
  -- ANCHOR: spec
  -- 作ったチームの数は、combinations で数えた数と同じ
  it "teams makes exactly combinations n k teams" $
    forAll (choose (0, 10 :: Int)) $ \n -> forAll (choose (0, n)) $ \k ->
      toInteger (length (teams k [1 .. n]))
        == combinations (toInteger n) (toInteger k)
  -- 4人から2人のチームは、この6組（中身も確かめる）
  it "teams makes the right teams" $
    teams 2 [1 .. 4 :: Int]
      `shouldBe` [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]
  -- n 人から k 人選ぶのと、残す n - k 人を選ぶのは同じ
  it "choosing k is the same as leaving n - k" $
    forAll (choose (0, 30 :: Integer)) $ \n -> forAll (choose (0, n)) $ \k ->
      combinations n k == combinations n (n - k)
  -- パスカルの式は、k = 0 や k = n の端でも成り立つ
  it "Pascal's rule holds at the ends too" $
    forAll (choose (1, 20 :: Integer)) $ \n -> forAll (choose (0, n)) $ \k ->
      combinations n k
        == combinations (n - 1) (k - 1) + combinations (n - 1) k
  -- サイコロ2つで合計 7 になる確率は 1/6
  it "the chance of 7 with two dice is 1/6" $
    probability (\(a, b) -> a + b == 7) twoDice `shouldBe` 1 % 6
  -- 23 人いれば、同じ誕生日の2人がいる確率は 50% を超える
  it "23 people are enough for a 50% birthday match" $
    (sameBirthday 365 22 < 0.5, sameBirthday 365 23 > 0.5)
      `shouldBe` (True, True)
  -- ANCHOR_END: spec

  -- 「トレーニング」
  describe "answers" $ do
    it "about 230 pulls for a 90% chance at 1%" $ do
      pullsFor 0.01 0.9 `shouldBe` Just 230
      pullsFor 0.1 0.1 `shouldBe` Just 1
      pullsFor 0.5 1 `shouldBe` Nothing
      pullsFor 0 0.9 `shouldBe` Nothing
    it "random gatherings of 23 match about half the time" $
      abs (birthdayTrials 23 10000 1 - sameBirthday 365 23) < 0.02
        `shouldBe` True
