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
  -- 比べる回数も数えている：2つなら1回、逆順なら挿入ソートは n(n-1)/2 回
  it "the sorts count their comparisons" $ do
    snd (mergeSortCount [2, 1]) `shouldBe` 1
    snd (insertionSortCount [2, 1]) `shouldBe` 1
    and [ snd (insertionSortCount [n, n - 1 .. 1]) == n * (n - 1) `div` 2
        | n <- [0 .. 50] ] `shouldBe` True
  -- ANCHOR_END: spec
  -- Int の端でも、数当ての真ん中は候補の外に出ない
  it "guesses stays inside at the edges of Int" $ do
    guesses maxBound maxBound maxBound `shouldBe` [maxBound]
    guesses minBound minBound minBound `shouldBe` [minBound]

  -- 「トレーニング」
  describe "answers" $ do
    it "money doubles in about 72 / percent years" $ do
      map yearsToDouble [0.01, 0.06, 0.12]
        `shouldBe` [Just 70, Just 12, Just 7]
      yearsToDouble 0 `shouldBe` Nothing
    it "levelFor undoes expFor" $
      forAll (choose (0, 100)) $ \lv -> levelFor (expFor lv) == lv
    it "levelFor at the edges of a level" $
      map levelFor [0, 99, 100, 199, 200] `shouldBe` [0, 0, 1, 1, 2]
