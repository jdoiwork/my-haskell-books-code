-- ANCHOR: all
-- ANCHOR: module
-- 🧪 Tests for the bag.
-- 袋のテスト
module BagSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- Maps, to make bags from what QuickCheck gives us.
-- QuickCheck がくれた値から袋を作るための Map
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Bag (Bag, addItem, removeItem, removeItemDraft)
-- ANCHOR_END: module

-- ANCHOR: toBag
-- 🎒 A bag where every count is 1 or more.
-- どの個数も 1 以上の袋
toBag :: Map String (Positive Int) -> Bag
toBag counts = Map.map getPositive counts
-- ANCHOR_END: toBag

-- ANCHOR: propDraft
-- 🔁 A round trip: put items in, take them out, and it is the same bag.
-- 往復：道具を入れて、出したら、もとの袋に戻る
prop_addRemoveDraft
  :: String -> Positive Int -> Map String (Positive Int) -> Bool
prop_addRemoveDraft item (Positive n) counts =
  let bag = toBag counts
  in  removeItemDraft item n (addItem item n bag) == Just bag
-- ANCHOR_END: propDraft

-- ANCHOR: spec
-- 🧪 The tests for the fixed removeItem.
-- 直した removeItem のテスト
spec :: Spec
spec =
  describe "removeItem" $
    -- 「addItem で入れたものを、取り出せる」
    prop "takes out what addItem put in" $
      \item (Positive n) counts ->
        let bag = toBag counts
        in  removeItem item n (addItem item n bag) `shouldBe` Just bag
-- ANCHOR_END: spec
-- ANCHOR_END: all
