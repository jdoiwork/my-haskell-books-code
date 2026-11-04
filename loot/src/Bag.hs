-- ANCHOR: all
-- ANCHOR: module
-- 🎒 The adventurer's bag: how many of each item.
-- 冒険者の袋：道具ごとの個数
module Bag
  ( Bag
  , emptyBag
  , addItem
  , removeItemDraft
  , removeItem
  ) where

-- A map from item names to counts.
-- 道具の名前から個数への Map
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- ANCHOR_END: module

-- ANCHOR: bag
-- 🎒 A bag: item name -> how many.
-- 袋：道具の名前 → 個数
type Bag = Map String Int

-- 🫙 An empty bag.
-- 空っぽの袋
emptyBag :: Bag
emptyBag = Map.empty

-- | ➕ Put some items into the bag.
-- 道具を袋に入れる
--
-- >>> addItem "potion" 2 emptyBag
-- fromList [("potion",2)]
addItem :: String -> Int -> Bag -> Bag
addItem item n bag = Map.insertWith (+) item n bag
-- ANCHOR_END: bag

-- ANCHOR: removeDraft
-- 📝 The first draft: take some items out of the bag.
--    Nothing if there are not enough.
-- 最初の下書き：道具を袋から出す。足りなければ Nothing
removeItemDraft :: String -> Int -> Bag -> Maybe Bag
removeItemDraft item n bag =
  let have = Map.findWithDefault 0 item bag
  in  if have < n
        then Nothing
        else Just (Map.insert item (have - n) bag)
-- ANCHOR_END: removeDraft

-- ANCHOR: remove
-- | ➖ Take some items out of the bag. Nothing if there are not enough.
--    When none are left, the item disappears from the bag.
-- 道具を袋から出す。足りなければ Nothing
-- 1つも残らなければ、その道具は袋から消える
--
-- >>> removeItem "potion" 2 (addItem "potion" 2 emptyBag)
-- Just (fromList [])
removeItem :: String -> Int -> Bag -> Maybe Bag
removeItem item n bag =
  let have = Map.findWithDefault 0 item bag
      left = have - n
  in  if left < 0
        then Nothing
        else if left == 0
          then Just (Map.delete item bag)
          else Just (Map.insert item left bag)
-- ANCHOR_END: remove
-- ANCHOR_END: all
