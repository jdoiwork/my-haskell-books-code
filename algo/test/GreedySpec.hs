module GreedySpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Algo.Greedy

-- 🎲 A few random quests within a day.
-- 1日の中の、でたらめな依頼をいくつか
genQuests :: Gen [Quest]
genQuests = do
  n <- chooseInt (0, 7)
  vectorOf n $ do
    s <- chooseInt (0, 20)
    len <- chooseInt (1, 8)
    pure (Quest "q" s (s + len))

spec :: Spec
spec = describe "Algo.Greedy" $ do
  -- ANCHOR: questProps
  -- 終わる順の貪欲は、全部試す仕様と同じ数を取れる
  it "greedy by end agrees with the spec" $ property $
    forAllShrink genQuests (shrinkList (const [])) $ \qs ->
      length (mostQuests qs) === mostQuestsSpec qs
  -- 短い順の貪欲は、まちがうことがある（QuickCheck が反例を見つける）
  it "greedy by length can be wrong" $
    property $ withNumTests 2000 $ expectFailure $
    forAllShrink genQuests (shrinkList (const [])) $ \qs ->
      length (mostQuestsBy (\q -> end q - start q) qs) === mostQuestsSpec qs
  -- ANCHOR_END: questProps
  -- 始まる順の貪欲も、まちがうことがある
  it "greedy by start can be wrong" $
    property $ withNumTests 2000 $ expectFailure $
    forAllShrink genQuests (shrinkList (const [])) $ \qs ->
      length (mostQuestsBy start qs) === mostQuestsSpec qs
  -- 例の依頼
  it "picks four quests from the example" $
    map title (mostQuests quests) `shouldBe`
      ["herbs", "goblins", "slimes", "letter"]
  -- ANCHOR: coinProps
  -- 1・5・10・25 なら、貪欲で正しい
  it "greedy coins are right for 1, 5, 10, 25" $ property $
    forAllShrink (chooseInt (0, 40)) shrink $ \a ->
      greedyCoins [1, 5, 10, 25] a === fewestCoinsSpec [1, 5, 10, 25] a
  -- 1・3・4 だと、貪欲はまちがう
  it "greedy coins are wrong for 1, 3, 4" $ property $ expectFailure $
    forAllShrink (chooseInt (0, 20)) shrink $ \a ->
      greedyCoins [1, 3, 4] a === fewestCoinsSpec [1, 3, 4] a
  -- ANCHOR_END: coinProps
  -- ハフマン符号：符号にして戻すと、もとの文
  it "decode undoes encode" $ property $
    forAll (listOf1 (elements "goblinsme ")) $ \text ->
      let tree = huffman text in decode tree (encode tree text) === text
