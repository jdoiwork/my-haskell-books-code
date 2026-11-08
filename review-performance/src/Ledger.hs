-- ANCHOR: all
-- 📒 The guild's ledger: who brought in how much gold.
-- ギルドの決算：誰が、どれだけの金貨を稼いだか
module Ledger
  ( Record (..)
  , parseRecords
  , totals
  , totalsNaive
  , totalsParallel
  , topThree
  ) where

-- ANCHOR: imports
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.ByteString.Char8       qualified as BS
import Data.List                   (sortOn)
import Data.Map.Strict             (Map)
import Data.Map.Strict             qualified as Map
import Data.Maybe                  (mapMaybe)
import Data.Ord                    (Down (..))
-- ANCHOR_END: imports

-- ANCHOR: record
-- 📝 One line of the ledger: "Lambda 120" (a hero and the gold).
-- 決算の1行：「Lambda 120」（勇者と金貨）
data Record = Record
  { hero :: !BS.ByteString
  , gold :: !Int
  }
  deriving (Show, Eq)

-- 📖 Read every line that has a name and a number; skip the rest.
-- 名前と数のそろった行を、全部読む（そろっていない行は飛ばす）
parseRecords :: BS.ByteString -> [Record]
parseRecords input = mapMaybe parseLine (BS.lines input)
  where
    parseLine line = case BS.words line of
      [name, amount] -> case BS.readInt amount of
        Just (g, rest) | BS.null rest -> Just (Record name g)
        _                             -> Nothing
      _ -> Nothing
-- ANCHOR_END: record

-- ANCHOR: totals
-- 💰 Each hero's total, added at once (Data.Map.Strict and foldl').
-- 勇者ごとの合計を、その場で足していく（Data.Map.Strict と foldl'）
totals :: [Record] -> Map BS.ByteString Int
totals records = foldl' add Map.empty records
  where
    add m r = Map.insertWith (+) (hero r) (gold r) m

-- 🐢 The same, by looking through the whole list for each hero.
-- 同じものを、勇者ごとにリスト全体を見直して
totalsNaive :: [Record] -> Map BS.ByteString Int
totalsNaive records =
  Map.fromList
    [ (h, sum [ gold r | r <- records, hero r == h ])
    | h <- map hero records ]
-- ANCHOR_END: totals

-- ANCHOR: parallel
-- ⚡ Split the records into parts, add up each part in parallel,
--    and put the parts together.
-- 記録をいくつかに分け、それぞれを並列に足して、最後にまとめる
totalsParallel :: Int -> [Record] -> Map BS.ByteString Int
totalsParallel parts records =
  Map.unionsWith (+) (parMap rdeepseq totals (chunks records))
  where
    size = max 1 (length records `div` parts + 1)
    chunks [] = []
    chunks xs = let (a, b) = splitAt size xs in a : chunks b
-- ANCHOR_END: parallel

-- ANCHOR: top
-- 🏆 The three richest heroes (ties: by name).
-- 稼ぎの多い3人（同じなら名前の順）
topThree :: Map BS.ByteString Int -> [(BS.ByteString, Int)]
topThree m = take 3 (sortOn (\(h, g) -> (Down g, h)) (Map.toList m))
-- ANCHOR_END: top
-- ANCHOR_END: all
