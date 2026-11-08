module StringsSpec (spec) where

import Data.Array (elems)
import Data.List (nub, sort)
import Test.Hspec
import Test.QuickCheck

import Algo.Strings
import AlgoAnswers (longestRun)

-- 🎲 Short strings over a small alphabet, so patterns really appear.
-- 小さな文字の集まりの、短い文字列（パターンが本当に現れるように）
small :: Gen String
small = listOf (elements "ab")

spec :: Spec
spec = describe "Algo.Strings" $ do
  -- 符号にして戻すと、もとの文字列
  it "unRunLength undoes runLength" $ property $ \s ->
    unRunLength (runLength s) === s
  -- ANCHOR: kmpProps
  -- KMP は、仕様と同じ位置を見つける
  it "findAll agrees with the spec" $ property $
    forAll ((,) <$> small `suchThat` (not . null) <*> small) $ \(pat, text) ->
      findAll pat text === findAllSpec pat text
  -- ANCHOR_END: kmpProps
  -- 表の例
  it "prefixTable" $
    elems (prefixTable "abcabd") `shouldBe` [0, 0, 0, 1, 2, 0]
  -- 補完
  it "complete" $ do
    complete "go" (fromWords monsters) `shouldBe`
      ["goblin", "golem", "gorgon"]
    complete "x" (fromWords monsters) `shouldBe` []
  -- トライに入れた単語は、全部、空の頭で補完でき、余計な単語は出てこない
  it "every word comes back" $ property $ \ws ->
    let ws' = filter (not . null) ws
    in  sort (complete "" (fromWords ws')) == nub (sort ws')
  -- トレーニングの解答
  it "longestRun" $ do
    longestRun "aaabccccdd" `shouldBe` Just ('c', 4)
    longestRun "aabb" `shouldBe` Just ('a', 2)
    longestRun "" `shouldBe` Nothing
  -- 2つに分けて作ったトライをまとめても、一度に作ったトライと同じ
  it "tries merge" $ property $ \as bs ->
    fromWords as <> fromWords bs === fromWords (as ++ bs)
