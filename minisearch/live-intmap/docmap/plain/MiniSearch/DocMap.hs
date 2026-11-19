-- ANCHOR: all
-- | A map keyed by DocId, kept as a bare IntMap (no size beside it):
--   the version that made the searches slower. Build it with
--   `cabal run minisearch-measure -f plain-size -- ...`.
-- DocId をキーにした入れ物。中は IntMap だけ（大きさを横に持たない）。
-- 検索が遅くなった版。`-f plain-size` を付けると、こちらで組まれる。
module MiniSearch.DocMap
  ( DocMap
  , empty, singleton, insert, fromList
  , lookup, findWithDefault
  , union, intersection
  , keys, elems, toList, size
  ) where

import Prelude hiding (lookup)

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import qualified Data.IntMap.Strict as IM

import MiniSearch.Types.DocId

-- 📦 Just the IntMap.
-- IntMap だけ
newtype DocMap a = DocMap (IM.IntMap a)
  deriving stock   (Eq, Show)
  deriving newtype (NFData, Binary, ToJSON, FromJSON, Functor)

-- ANCHOR: size
-- 📏 IntMap.size counts every entry, each time it is called: O(n).
-- IntMap.size は、呼ぶたびに全部を数える：O(n)
size :: DocMap a -> Int
size (DocMap m) = IM.size m
-- ANCHOR_END: size

key :: DocId -> Int
key (DocId w) = fromIntegral w

docIdOf :: Int -> DocId
docIdOf = DocId . fromIntegral

empty :: DocMap a
empty = DocMap IM.empty

singleton :: DocId -> a -> DocMap a
singleton i x = DocMap (IM.singleton (key i) x)

insert :: DocId -> a -> DocMap a -> DocMap a
insert i x (DocMap m) = DocMap (IM.insert (key i) x m)

fromList :: [(DocId, a)] -> DocMap a
fromList xs = DocMap (IM.fromList [ (key i, x) | (i, x) <- xs ])

lookup :: DocId -> DocMap a -> Maybe a
lookup i (DocMap m) = IM.lookup (key i) m

findWithDefault :: a -> DocId -> DocMap a -> a
findWithDefault x i (DocMap m) = IM.findWithDefault x (key i) m

union :: DocMap a -> DocMap a -> DocMap a
union (DocMap a) (DocMap b) = DocMap (IM.union a b)

intersection :: DocMap a -> DocMap b -> DocMap a
intersection (DocMap a) (DocMap b) = DocMap (IM.intersection a b)

keys :: DocMap a -> [DocId]
keys (DocMap m) = map docIdOf (IM.keys m)

elems :: DocMap a -> [a]
elems (DocMap m) = IM.elems m

toList :: DocMap a -> [(DocId, a)]
toList (DocMap m) = [ (docIdOf i, x) | (i, x) <- IM.toList m ]
-- ANCHOR_END: all
