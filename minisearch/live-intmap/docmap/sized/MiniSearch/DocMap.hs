-- ANCHOR: all
-- | A map keyed by DocId, kept as an IntMap, with its size beside it.
--   The functions have the same names as Data.Map's.
-- DocId をキーにした入れ物。中は IntMap で、大きさを横に持つ。
-- 関数は Data.Map と同じ名前にしてある。
module MiniSearch.DocMap
  ( DocMap
  , empty, singleton, insert, fromList
  , lookup, findWithDefault
  , union, intersection
  , keys, elems, toList, size
  ) where

import Prelude hiding (lookup)

import Control.DeepSeq (NFData (..))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Binary (Binary (..))
import qualified Data.IntMap.Strict as IM

import MiniSearch.Types.DocId

-- ANCHOR: type
-- 📦 The IntMap, and how many entries it has. IntMap.size counts
--    every entry each time (O(n)); Map.size just reads it (O(1)).
--    So we keep the size ourselves.
-- IntMap と、その要素の数。IntMap.size は毎回全部を数える（O(n)）。
-- Map.size は覚えている数を読むだけ（O(1)）。だから、自分で覚えておく
data DocMap a = DocMap !Int !(IM.IntMap a)
  deriving stock (Eq, Show)

-- 📏 Make a DocMap, counting the entries once.
-- 要素を一度だけ数えて、DocMap にする
sized :: IM.IntMap a -> DocMap a
sized m = DocMap (IM.size m) m

-- 📏 How many entries: read, not counted.
-- 要素の数：数えずに、読むだけ
size :: DocMap a -> Int
size (DocMap n _) = n
-- ANCHOR_END: type

-- ANCHOR: key
-- 🔢 A DocId holds a Word64; an Int has the same 64 bits.
--    fromIntegral keeps the bits (big ids become negative Ints).
-- DocId の中は Word64。Int も同じ64ビット。
-- fromIntegral はビットをそのまま写す（大きな番号は負の Int になる）
key :: DocId -> Int
key (DocId w) = fromIntegral w

docIdOf :: Int -> DocId
docIdOf = DocId . fromIntegral
-- ANCHOR_END: key

instance NFData a => NFData (DocMap a) where
  rnf (DocMap _ m) = rnf m

instance Functor DocMap where
  fmap f (DocMap n m) = DocMap n (fmap f m)

-- 💾 Saved as the IntMap alone; the size is counted again when read.
-- 保存するのは IntMap だけ。大きさは、読むときに数え直す
instance Binary a => Binary (DocMap a) where
  put (DocMap _ m) = put m
  get              = sized <$> get

instance ToJSON a => ToJSON (DocMap a) where
  toJSON (DocMap _ m) = toJSON m

instance FromJSON a => FromJSON (DocMap a) where
  parseJSON v = sized <$> parseJSON v

empty :: DocMap a
empty = DocMap 0 IM.empty

singleton :: DocId -> a -> DocMap a
singleton i x = DocMap 1 (IM.singleton (key i) x)

-- ➕ Add one; the size grows only if the id was not there.
-- 1つ足す。大きさが増えるのは、その番号がなかったときだけ
insert :: DocId -> a -> DocMap a -> DocMap a
insert i x (DocMap n m) =
  case IM.insertLookupWithKey (\_ new _ -> new) (key i) x m of
    (Nothing, m') -> DocMap (n + 1) m'
    (Just _,  m') -> DocMap n m'

fromList :: [(DocId, a)] -> DocMap a
fromList xs = sized (IM.fromList [ (key i, x) | (i, x) <- xs ])

lookup :: DocId -> DocMap a -> Maybe a
lookup i (DocMap _ m) = IM.lookup (key i) m

findWithDefault :: a -> DocId -> DocMap a -> a
findWithDefault x i (DocMap _ m) = IM.findWithDefault x (key i) m

-- ANCHOR: union
-- 🧩 Put two together. The size of the result is counted once,
--    here (Index's <> does this for each chunk).
-- 2つを合わせる。結果の大きさは、ここで1回数える
-- （Index の <> が、かたまりごとに呼ぶ）
union :: DocMap a -> DocMap a -> DocMap a
union (DocMap _ a) (DocMap _ b) = sized (IM.union a b)

intersection :: DocMap a -> DocMap b -> DocMap a
intersection (DocMap _ a) (DocMap _ b) = sized (IM.intersection a b)
-- ANCHOR_END: union

keys :: DocMap a -> [DocId]
keys (DocMap _ m) = map docIdOf (IM.keys m)

elems :: DocMap a -> [a]
elems (DocMap _ m) = IM.elems m

toList :: DocMap a -> [(DocId, a)]
toList (DocMap _ m) = [ (docIdOf i, x) | (i, x) <- IM.toList m ]
-- ANCHOR_END: all
