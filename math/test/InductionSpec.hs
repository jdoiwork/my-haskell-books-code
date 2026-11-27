module InductionSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import InductionAnswers
import Math.Induction

-- 🎲 A random dungeon map. n is halved for each side, to keep it shallow
--    (it is not an exact limit on the number of forks).
-- でたらめな地図。左右に n を半分ずつ渡して、深くなりすぎないようにする
-- （分かれ道の数の、ぴったりの上限ではない）
genRoom :: Int -> Gen Room
genRoom 0 = pure DeadEnd
genRoom n = frequency
  [ (1, pure DeadEnd)
  , (3, Fork <$> genRoom (n `div` 2) <*> genRoom (n `div` 2))
  ]

spec :: Spec
spec = do
  -- ANCHOR: spec
  -- 階段の和と、公式は同じ
  it "sumTo agrees with the formula" $
    forAll (choose (0, 1000)) $ \n -> sumTo n == sumFormula n
  -- 行き止まりは、いつも分かれ道より1つ多い
  it "a map has one more dead end than forks" $
    forAll (sized genRoom) $ \room -> deadEnds room == forks room + 1
  -- 速いべき乗は、ふつうの ^ と同じ
  it "power agrees with (^)" $
    forAll (choose (-5, 5)) $ \x -> forAll (choose (0, 40)) $ \n ->
      power x n == x ^ n
  -- 下書きは、どこかでまちがえる
  it "the draft gets some power wrong" $
    expectFailure $ forAll (choose (2, 5)) $ \x ->
      forAll (choose (0, 40)) $ \n -> powerDraft x n == x ^ n
  -- オイラーの式は、39 までは素数
  it "Euler's numbers are prime up to 39" $
    forAll (choose (0, 39)) $ \n -> isPrime (eulerNumber n)
  -- でも、40 で破れる
  it "but not at 40" $
    isPrime (eulerNumber 40) `shouldBe` False
  -- ANCHOR_END: spec

  -- 「トレーニング」
  describe "answers" $ do
    -- ANCHOR: oddsTest
    -- 最初の n 個の奇数の和は n の2乗（n は 0 以上）
    it "the first n odd numbers add up to n squared" $
      forAll (choose (0, 1000)) $ \n -> sumOdds n == n * n
    -- ANCHOR_END: oddsTest
    -- ANCHOR: deepestTest
    -- 行き止まりは 2 ^ deepest 個まで。2 ^ 63 は Int からあふれるので、
    -- 両辺を Integer にしてくらべる
    it "a map has at most 2 ^ deepest dead ends" $
      forAll (sized genRoom) $ \room ->
        toInteger (deadEnds room) <= (2 :: Integer) ^ deepest room
    -- 片側だけ 63 段の地図：行き止まりは 64 個、2 ^ 63 よりずっと少ない
    it "a map deep on one side only" $ do
      let room = iterate (Fork DeadEnd) DeadEnd !! 63
      (deepest room, deadEnds room) `shouldBe` (63, 64)
      toInteger (deadEnds room) <= (2 :: Integer) ^ deepest room
        `shouldBe` True
    -- ANCHOR_END: deepestTest
