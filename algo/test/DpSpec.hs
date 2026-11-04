module DpSpec (spec) where

import Data.List (subsequences)
import Test.Hspec
import Test.QuickCheck

import Algo.Dp
import AlgoAnswers (gridPaths, lcsLength)

spec :: Spec
spec = describe "Algo.Dp" $ do
  -- フィボナッチの最初のほう
  it "fibs" $ take 10 fibs `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
  -- 表を使っても、仕様と同じ数
  it "ways agrees with the spec" $ property $
    forAll (chooseInt (0, 60)) $ \a ->
      ways [1, 5, 10, 25] a === waysSpec [1, 5, 10, 25] a
  -- 編集距離も、仕様と同じ
  it "editDistance agrees with the spec" $ property $
    forAll (shortName >*< shortName) $ \(s, t) ->
      editDistance s t === editDistanceSpec s t
  -- ナップサックは、宝の選び方を全部試したものと同じ
  it "knapsack agrees with trying every choice" $ property $
    forAll (chooseInt (0, 18)) $ \w ->
      knapsack treasures w ===
        maximum [ sum (map value pick)
                | pick <- subsequences treasures
                , sum (map weight pick) <= w ]
  -- トレーニングの解答
  it "lcsLength" $ do
    lcsLength "goblin" "gobiln" `shouldBe` 5
    lcsLength "slime" "lime" `shouldBe` 4
  it "gridPaths" $ do
    gridPaths ["...", "...", "..."] `shouldBe` 6
    gridPaths ["...", ".#.", "..."] `shouldBe` 2
  where
    shortName = resize 6 (listOf (elements "abc"))
    a >*< b = (,) <$> a <*> b
