-- ANCHOR: all
{-# OPTIONS_GHC -Wno-orphans #-}

-- ANCHOR: module
-- 🧪 Tests for saving monsters as JSON.
-- モンスターを JSON で保存するテスト
module MonsterSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- JSON and text.
-- JSON とテキスト
import Data.Aeson (decode, encode)
import qualified Data.Text as T

import Monster (Element (..), Monster (..))
-- ANCHOR_END: module

-- ANCHOR: arbitrary
-- 🎲 A random element: one of all the elements.
-- 属性をでたらめに：全部の属性のどれか
instance Arbitrary Element where
  arbitrary = arbitraryBoundedEnum

-- 🎲 A random monster. Its name and drops can be any text at all.
-- モンスターをでたらめに。名前もドロップ品も、どんな文字列でもよい
instance Arbitrary Monster where
  arbitrary = do
    monsterName    <- arbitrary
    monsterHp      <- chooseInt (1, 999)
    monsterElement <- arbitrary
    monsterDrops   <- arbitrary
    pure Monster
      { name    = T.pack monsterName
      , hp      = monsterHp
      , element = monsterElement
      , drops   = map T.pack monsterDrops
      }
-- ANCHOR_END: arbitrary

-- ANCHOR: spec
-- 🔁 Write a monster as JSON, read it back: the same monster.
-- モンスターを JSON に書いて、読み戻すと、同じモンスター
spec :: Spec
spec =
  describe "Monster JSON" $
    -- 「JSON に書いて読み戻すと、もとのモンスターに戻る」
    prop "decode (encode m) == Just m" $
      \monster ->
        decode (encode monster) `shouldBe` Just (monster :: Monster)
-- ANCHOR_END: spec
-- ANCHOR_END: all
