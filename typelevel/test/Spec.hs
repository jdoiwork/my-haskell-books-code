module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Chest
import Gate
import TypeAnswers
import Vec

main :: IO ()
main = hspec $ do
  describe "Chest" $
    -- 鍵を外してから開ける
    it "unlock, then open" $
      open (unlock (newChest ["potion", "sword"])) `shouldBe`
        ["potion", "sword"]
  describe "Vec" $ do
    -- 先頭と、組にする
    it "vhead and vzip" $ do
      vhead party `shouldBe` "Lambda"
      vtoList (vzip party ("sword" :> "staff" :> "shield" :> VNil))
        `shouldBe`
          [("Lambda", "sword"), ("Python", "staff"), ("Rust", "shield")]
    -- withVec で作っても、リストに戻すと同じ
    it "withVec keeps the list" $ property $ \xs ->
      withVec (xs :: [Int]) vtoList == xs
    -- つないだ長さは、長さの和
    it "vappend adds the lengths" $ property $ \xs ys ->
      withVec (xs :: [Int]) (\v -> withVec ys (\w -> vlength (vappend v w)))
        == length xs + length ys
    -- 長さが3のときだけ、門を通る
    it "toThree" $ property $ \xs ->
      fmap vtoList (toThree (xs :: [Int])) ==
        (if length xs == 3 then Just xs else Nothing)
  describe "answers" $ do
    -- ステップ2：関数で組み合わせる
    it "vzipWith" $
      vtoList (vzipWith (++) party ("!" :> "?" :> "." :> VNil))
        `shouldBe` ["Lambda!", "Python?", "Rust."]
    -- ステップ3：先頭の2つを入れ替える
    it "swapFirstTwo" $
      vtoList (swapFirstTwo party) `shouldBe` ["Python", "Lambda", "Rust"]
