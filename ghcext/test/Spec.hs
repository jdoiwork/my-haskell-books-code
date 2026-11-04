{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson (encode)
import Test.Hspec

import Ado
import ExtAnswers
import qualified Labels as L
import Money
import Records
import qualified Shop
import Strings

main :: IO ()
main = hspec $ do
  describe "OrPatterns" $
    -- 1行にまとめても、同じ分け方
    it "category agrees with categoryOld" $
      map Shop.category items `shouldBe` map Shop.categoryOld items
  describe "LambdaCase" $
    -- \case で書いた説明
    it "describe" $
      Shop.describe [Just 50, Nothing] `shouldBe` ["50 G", "sold out"]
  describe "deriving" $ do
    -- Sum から借りた <> は足し算
    it "Gold adds up" $
      mconcat [Gold 10, Gold 5, Gold 1] `shouldBe` Gold 16
    -- Max から借りた <> は大きいほう
    it "Best keeps the larger" $
      Best 30 <> Best 80 <> Best 50 `shouldBe` Best 80
    -- anyclass の ToJSON
    it "Loot to JSON" $
      encode (Loot "potion" 3) `shouldBe` "{\"count\":3,\"item\":\"potion\"}"
  describe "records" $ do
    -- 同じ名前のフィールドを、ドットで読み分ける
    it "battleLine" $
      battleLine lambda slime `shouldBe` "Lambda (30) vs Slime (8)"
    -- Hero{..} で、全部のフィールドが使える
    it "heal" $ heal lambda `shouldBe` "Lambda heals to 40"
  describe "labels" $ do
    -- #name は Column "name"
    it "columns" $ L.columns `shouldBe` [L.Column "name", L.Column "level"]
    -- #hp は、フィールドを読む関数
    it "getters" $ (L.heroHp, L.heroName) `shouldBe` (30, "Lambda")
  describe "ApplicativeDo" $
    -- ZipList でも do が使える
    it "sums" $ sums `shouldBe` [11, 22, 33]
  describe "MultilineStrings" $
    -- 共通の字下げと、最初の改行が取りのぞかれる
    it "poem" $ poem `shouldBe` "Roses are red,\n  violets are blue."
  describe "answers" $
    -- ステップ2：Min から借りた <> は小さいほう
    it "Lowest" $ Lowest 300 <> Lowest 50 <> Lowest 120 `shouldBe` Lowest 50
  where
    items = ["potion", "elixir", "sword", "axe", "herb"]
