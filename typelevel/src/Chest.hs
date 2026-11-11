-- ANCHOR: all
-- ANCHOR: module
-- | A treasure chest whose state (locked or not) lives in its type.
--   Only the type Chest is exported, not its constructor: the only ways
--   to make or change a chest are the functions below.
-- 状態（鍵がかかっているか）を、型に持つ宝箱。
-- 外に見せるのは型の Chest だけで、コンストラクターは見せない：
-- 宝箱を作ったり変えたりできるのは、下の関数だけ
module Chest
  ( Lock (..)
  , Chest
  , newChest
  , unlock
  , open
  , lock
  ) where
-- ANCHOR_END: module

-- ANCHOR: chest
-- 🔒 Two states. With DataKinds, they can also be used as types.
-- 2つの状態。DataKinds があれば、型としても使える
data Lock = Locked | Unlocked

-- 🧰 A chest of items. s is a type-level Lock: Locked or Unlocked.
-- アイテムの入った宝箱。s は型の世界の Lock：Locked か Unlocked
newtype Chest (s :: Lock) = Chest [String]

-- 🛑 Do not let coerce turn a Chest Locked into a Chest Unlocked.
-- coerce で Chest Locked を Chest Unlocked に変えさせない
type role Chest nominal

newChest :: [String] -> Chest Locked
newChest = Chest

-- 🗝️ Only a locked chest can be unlocked, and only an unlocked one opened.
-- 開けられるのは、鍵を外した宝箱だけ
unlock :: Chest Locked -> Chest Unlocked
unlock (Chest items) = Chest items

open :: Chest Unlocked -> [String]
open (Chest items) = items

lock :: Chest Unlocked -> Chest Locked
lock (Chest items) = Chest items
-- ANCHOR_END: chest
-- ANCHOR_END: all
