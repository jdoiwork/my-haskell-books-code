-- ANCHOR: all
-- ANCHOR: module
-- 💰 Splitting the loot among the party.
-- 戦利品のゴールドを、パーティーで分ける
module Loot
  ( splitLootDraft
  , splitLoot
  ) where
-- ANCHOR_END: module

-- ANCHOR: draft
-- | 📝 The first draft: everyone gets the same share.
-- 最初の下書き：全員に同じ分け前を配る
--
-- >>> splitLootDraft 100 4
-- [25,25,25,25]
splitLootDraft :: Int -> Int -> [Int]
splitLootDraft gold members =
  let share = gold `div` members
  in  replicate members share
-- ANCHOR_END: draft

-- ANCHOR: fixed
-- | 💰 Split the gold among the members, losing nothing.
--    The remainder goes one by one to the first members.
--    The number of members must be 1 or more.
-- ゴールドをメンバーで分ける。1ゴールドもなくさない
-- 余りは、前のメンバーから1ずつ配る
-- メンバーは1人以上であること
--
-- >>> splitLoot 100 4
-- [25,25,25,25]
--
-- >>> splitLoot 10 3
-- [4,3,3]
splitLoot :: Int -> Int -> [Int]
splitLoot gold members =
  let share = gold `div` members
      extra = gold `mod` members
  in  replicate extra (share + 1) ++ replicate (members - extra) share
-- ANCHOR_END: fixed
-- ANCHOR_END: all
