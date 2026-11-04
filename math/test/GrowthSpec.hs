module GrowthSpec (spec) where

import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

import GrowthAnswers
import Math.Growth

spec :: Spec
spec = do
  -- ANCHOR: spec
  -- halvings は、2 の何乗のあいだにあるかを答える
  it "2 ^ halvings n <= n < 2 ^ (halvings n + 1)" $
    forAll (choose (1, 10 ^ (9 :: Int))) $ \n ->
      let h = halvings n
      in  2 ^ h <= n && n < 2 ^ (h + 1)
  -- 数当ては、いつも当たり、halvings n + 1 回以内で済む
  it "the guessing game always wins in halvings n + 1 guesses" $
    forAll (choose (1, 100000)) $ \n -> forAll (choose (1, n)) $ \t ->
      let gs = guesses 1 n t
      in  last gs == t && length gs <= halvings n + 1
  -- マージソートは並べ、比べる回数は n * (halvings n + 1) 以下
  it "merge sort sorts, with at most n * (log2 n + 1) comparisons" $
    property $ \xs ->
      let (sorted, count) = mergeSortCount xs
          n               = length xs
      in  sorted == sort xs && count <= n * (halvings n + 1)
  -- 挿入ソートも並べる
  it "insertion sort sorts too" $
    property $ \xs -> fst (insertionSortCount xs) == sort xs
  -- ANCHOR_END: spec

  -- 「トレーニング」
  describe "answers" $ do
    it "money doubles in about 72 / percent years" $
      map yearsToDouble [0.01, 0.06, 0.12] `shouldBe` [70, 12, 7]
    it "levelFor undoes expFor" $
      forAll (choose (1, 40)) $ \lv -> levelFor (expFor lv) == lv
