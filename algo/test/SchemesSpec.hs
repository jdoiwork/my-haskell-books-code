module SchemesSpec (spec) where

import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

import Algo.Schemes
import AlgoAnswers (collatzSteps, depthAlg)

spec :: Spec
spec = describe "Algo.Schemes" $ do
  -- 広げてから畳むと、もとのリスト
  it "toList' undoes fromList" $ property $ \xs ->
    toList' (fromList xs) === (xs :: [Int])
  -- cata で、sum と length
  it "sum and length as algebras" $ property $ \xs ->
    cata sumAlg (fromList xs) === sum xs
      .&&. cata lengthAlg (fromList (xs :: [Int])) === length xs
  -- 計算式
  it "evaluates and shows 2 * (3 + 4)" $ do
    cata evalAlg twoTimes `shouldBe` 14
    cata showAlg twoTimes `shouldBe` "2 * (3 + 4)"
  -- hylo のマージソートは、sort と同じ
  it "msortHylo agrees with sort" $ property $ \xs ->
    msortHylo xs === sort (xs :: [Int])
  -- 階乗
  it "factorial" $ map factorial [0 .. 5] `shouldBe` [1, 1, 2, 6, 24, 120]
  -- 21! は Int からあふれる大きさ。Integer なので正しく出る
  it "factorial 21 does not overflow" $
    factorial 21 `shouldBe` product [1 .. 21]
  -- トレーニングの解答
  it "depthAlg" $ cata depthAlg twoTimes `shouldBe` 3
  it "collatzSteps" $ map collatzSteps [1, 6, 27] `shouldBe` [0, 8, 111]
  -- 最初の 3n + 1 が Int からあふれる種（数学の章の Integer 版も 484）
  it "collatzSteps does not overflow" $
    collatzSteps 6148914691236517207 `shouldBe` 484
