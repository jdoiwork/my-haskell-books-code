{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Lens
import Data.Aeson.Lens (key, _Integer)
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck

import Guild
import LensAnswers
import qualified MiniLens as Mini
import qualified Plain
import SaveData

-- 🎯 The path used in the law tests.
-- 法則のテストで使う道順
leaderHp :: Lens' Party Int
leaderHp = leader . stats . hp

main :: IO ()
main = hspec $ do
  describe "plain records and lenses agree" $
    -- 手で作り直しても、レンズでも、同じ結果
    it "knockOutLeader" $ do
      Plain.leaderHp (Plain.knockOutLeader Plain.party) `shouldBe` 0
      knockOutLeader party ^. leaderHp `shouldBe` 0
  -- ANCHOR: laws
  describe "lens laws" $ do
    -- 入れたものを読むと、入れたものが出てくる
    it "view after set" $ property $ \v ->
      view leaderHp (set leaderHp v party) == v
    -- 読んだものを入れ直しても、何も変わらない
    it "set what you viewed" $
      set leaderHp (view leaderHp party) party `shouldBe` party
    -- 2回入れたら、あとのほうだけが残る
    it "set twice" $ property $ \v w ->
      set leaderHp w (set leaderHp v party) == set leaderHp w party
  -- ANCHOR_END: laws
  describe "Guild" $ do
    -- 仲間全員のレベルが1つ上がる
    it "levelUpAll" $
      levelUpAll party ^.. members . traversed . stats . level
        `shouldBe` [5, 7]
    -- Map の中をのぞいて、ポーションを足す
    it "givePotion and potionsOf" $ do
      givePotion party ^. leader . bag `shouldBe`
        Map.fromList [("herb", 2), ("potion", 1)]
      map potionsOf (party ^. members) `shouldBe` [1, 0]
    -- 1人目がいなければ Nothing
    it "firstMemberName" $ do
      firstMemberName party `shouldBe` Just "Python"
      firstMemberName (party & members .~ []) `shouldBe` Nothing
    -- 全員の HP の合計
    it "totalHp" $ totalHp party `shouldBe` 95
  describe "MiniLens" $ do
    -- 小さなレンズも、法則を守る
    it "view after set" $ property $ \a b v ->
      Mini.view Mini.px (Mini.set Mini.px v (Mini.Point a b)) == v
    -- traverse は、そのままトラバーサルとして使える
    it "over traverse" $ property $ \xs ->
      Mini.over traverse (* 2) xs == map (* 2) (xs :: [Int])
  describe "SaveData" $ do
    -- 3段下の値をのぞく
    it "heroLevel" $ do
      heroLevel saveJson `shouldBe` Just 12
      heroLevel "{\"hero\": {}}" `shouldBe` Nothing
    -- アイテムの名前を全部
    it "itemNames" $ itemNames saveJson `shouldBe` ["potion", "herb"]
    -- ゴールドだけが 100 増える
    it "richer" $ do
      let gold = key "hero" . key "stats" . key "gold" . _Integer
      (richer saveJson >>= (^? gold)) `shouldBe` Just 350
  describe "answers" $ do
    -- ステップ2：全員にポーション
    it "potionForEveryone" $
      potionForEveryone party ^.. members . traversed . bag `shouldBe`
        [Map.fromList [("potion", 2)], Map.fromList [("potion", 1)]]
    -- ステップ3：3個より多いアイテム
    it "bigItems" $ bigItems saveJson `shouldBe` ["herb"]
