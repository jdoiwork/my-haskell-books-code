-- ANCHOR: all
-- QuickCheck needs to know how to make a Member. We teach it here, in the
-- tests, not in the Party module (GHC calls this an "orphan instance").
-- QuickCheck に Member の作り方を教える。Party モジュールではなく、
-- テストの中に書く（GHC はこれを「孤児インスタンス」と呼んで警告する）
{-# OPTIONS_GHC -Wno-orphans #-}

-- ANCHOR: module
-- 🧪 Tests for the party.
-- パーティーのテスト
module PartySpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Party (Member (..), rankByLevel)
-- ANCHOR_END: module

-- ANCHOR: arbitrary
-- 🎲 How to make a random member: one of four names, level 1 to 99.
-- メンバーをでたらめに作る方法：4つの名前のどれか、レベルは 1〜99
instance Arbitrary Member where
  arbitrary = do
    memberName  <- elements ["Lambda", "Python", "TS", "Rust"]
    memberLevel <- chooseInt (1, 99)
    pure (Member memberName memberLevel)
-- ANCHOR_END: arbitrary

-- ANCHOR: spec
-- 🧪 Ranking twice is the same as ranking once.
-- 2回並べても、1回並べたのと同じ
spec :: Spec
spec =
  describe "rankByLevel" $
    -- 「2回並べても、1回並べたのと同じ」
    prop "ranking twice is the same as ranking once" $
      \members ->
        rankByLevel (rankByLevel members) `shouldBe` rankByLevel members
-- ANCHOR_END: spec
-- ANCHOR_END: all
