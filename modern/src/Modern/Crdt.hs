-- ANCHOR: all
-- | CRDTs: data that two offline devices can change on their own
--   and merge later, in any order, any number of times.
-- CRDT：2台の端末がネットワークなしで別々に変えても、あとで
-- どの順に、何度合わせても、同じところに落ち着くデータ
module Modern.Crdt
  ( -- * A counter that only grows
    GCounter (..)
  , increment
  , counterValue
  , AddCounter (..)
    -- * Last writer wins
  , Lww (..)
  , LwwDraft (..)
    -- * A set where adding wins
  , Tag
  , OrSet (..)
  , emptySet
  , add
  , remove
  , members
  ) where

-- ANCHOR: imports
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
-- ANCHOR_END: imports

-- ANCHOR: gcounter
-- 🔢 A counter that only grows: each device keeps its own count.
-- 増えるだけのカウンター：端末ごとに、自分の数を持つ
newtype GCounter = GCounter (Map String Int)
  deriving (Show, Eq)

-- ➕ One more, counted on this device.
-- この端末で、1つ数える
increment :: String -> GCounter -> GCounter
increment device (GCounter counts) =
  GCounter (Map.insertWith (+) device 1 counts)

-- 🧮 The value is the sum of every device's count.
-- 値は、全部の端末の数の合計
counterValue :: GCounter -> Int
counterValue (GCounter counts) = sum (Map.elems counts)

-- 🤝 Merge: for each device, keep the larger count.
-- 合わせる：端末ごとに、大きいほうの数を残す
instance Semigroup GCounter where
  GCounter a <> GCounter b = GCounter (Map.unionWith max a b)
-- ANCHOR_END: gcounter

-- ANCHOR: addCounter
-- 😵 A first draft: merge by adding the counts.
-- 最初の下書き：数を足して合わせる
newtype AddCounter = AddCounter (Map String Int)
  deriving (Show, Eq)

instance Semigroup AddCounter where
  AddCounter a <> AddCounter b = AddCounter (Map.unionWith (+) a b)
-- ANCHOR_END: addCounter

-- ANCHOR: lww
-- ✍️ Last writer wins: keep the value with the later stamp.
--    On a tie, the device name, and then the value itself, decide,
--    so every device picks the same one.
-- 最後に書いた者が勝つ：時刻の新しい値を残す。
-- 同じ時刻なら端末の名前で、それも同じなら値そのもので決める。
-- どの端末でも、同じほうを選ぶように
data Lww a = Lww
  { stamp  :: Int
  , device :: String
  , value  :: a
  }
  deriving (Show, Eq)

instance Ord a => Semigroup (Lww a) where
  a <> b
    | key a >= key b = a
    | otherwise      = b
    where
      key w = (stamp w, device w, value w)
-- ANCHOR_END: lww

-- ANCHOR: lwwDraft
-- 😵 A first draft: on a tie, keep the left one.
-- 最初の下書き：同じ時刻なら、左を残す
newtype LwwDraft a = LwwDraft (Lww a)
  deriving (Show, Eq)

instance Semigroup (LwwDraft a) where
  LwwDraft a <> LwwDraft b
    | stamp a >= stamp b = LwwDraft a
    | otherwise          = LwwDraft b
-- ANCHOR_END: lwwDraft

-- ANCHOR: orset
-- 🏷️ Every add gets a fresh tag: (device, number).
-- 追加のたびに、新しい札を付ける：（端末, 番号）
type Tag = (String, Int)

-- 📋 A set where adding wins: an item is in the set while it has
--    a tag that has not been removed.
-- 追加が勝つ集合：消されていない札が1枚でもあれば、集合に入っている
--    The fields are strict (!): making an OrSet computes its maps.
-- フィールドは正格（!）：OrSet を作ると、中の Map も計算される
data OrSet a = OrSet
  { tags    :: !(Map a (Set Tag))
  , removed :: !(Set Tag)
  }
  deriving (Show, Eq)

emptySet :: OrSet a
emptySet = OrSet Map.empty Set.empty

-- ➕ Add with a new tag.
-- 新しい札を付けて追加する
add :: Ord a => Tag -> a -> OrSet a -> OrSet a
add tag item set =
  let more = Map.insertWith Set.union item (Set.singleton tag) (tags set)
  in set { tags = more }

-- ➖ Remove: put every tag we have seen for it on the removed list.
-- 消す：その品物について、見たことのある札を全部「消した」に入れる
remove :: Ord a => a -> OrSet a -> OrSet a
remove item set =
  let seen = Map.findWithDefault Set.empty item (tags set)
  in set { removed = removed set <> seen }

-- 👀 The items that still have a living tag.
-- 生きている札が残っている品物
members :: OrSet a -> Set a
members set =
  Map.keysSet (Map.filter alive (tags set))
  where
    alive itemTags = not (Set.null (itemTags Set.\\ removed set))

-- 🤝 Merge: put all the tags together.
-- 合わせる：札を全部まとめる
instance Ord a => Semigroup (OrSet a) where
  OrSet t1 r1 <> OrSet t2 r2 =
    OrSet (Map.unionWith Set.union t1 t2) (r1 <> r2)
-- ANCHOR_END: orset
-- ANCHOR_END: all
