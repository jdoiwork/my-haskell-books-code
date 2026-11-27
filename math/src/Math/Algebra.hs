-- ANCHOR: all
-- | Associativity and monoids: why we may split a calculation up.
-- 結合法則とモノイド：計算を分けてよい理由
module Math.Algebra
  ( Summary (..)
  , summarize
  , meanOf
  , chunksOf
  , summarizeInChunks
  , meanOfMeans
  , Visits (..)
  , visit
  ) where

-- ANCHOR: imports
import Data.Set (Set)
import qualified Data.Set as Set
-- ANCHOR_END: imports

-- ANCHOR: summary
-- 📊 A summary of response times (in whole milliseconds) that can be
--    combined: how many, the total, the smallest and the largest.
--    Whole numbers, so adding them up never rounds (Double would).
-- 合わせられる、応答時間（ミリ秒の整数）のまとめ：個数、合計、
-- いちばん小さい値と大きい値。整数なので、足しても丸めない
data Summary = Summary
  { count   :: Int
  , total   :: Int
  , fastest :: Int
  , slowest :: Int
  }
  deriving (Show, Eq)

-- 🤝 Combining two summaries. (a <> b) <> c == a <> (b <> c).
-- 2つのまとめを合わせる
instance Semigroup Summary where
  a <> b = Summary
    { count   = count a + count b
    , total   = total a + total b
    , fastest = min (fastest a) (fastest b)
    , slowest = max (slowest a) (slowest b)
    }

-- 🫙 The summary of nothing: it changes nothing when combined.
-- 何もないもののまとめ：合わせても、何も変えない
instance Monoid Summary where
  mempty = Summary 0 0 maxBound minBound

-- 📏 The summary of one response time.
-- 応答時間1つのまとめ
summarize :: Int -> Summary
summarize t = Summary 1 t t t

-- ⚖️ The mean, from a summary. Nothing for the summary of nothing.
-- まとめから、平均を出す。何もないもののまとめには、平均がない
meanOf :: Summary -> Maybe Double
meanOf s
  | count s == 0 = Nothing
  | otherwise    = Just (fromIntegral (total s) / fromIntegral (count s))
-- ANCHOR_END: summary

-- ANCHOR: chunks
-- ✂️ Cut a list into pieces of n (like the logs of several servers).
--    n must be 1 or more: with 0, splitAt would cut off nothing forever.
-- リストを n 個ずつに切る（何台ものサーバーのログのように）。
-- n は 1 以上（0 だと、splitAt が何も切り取らずに、いつまでも続く）
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | n <= 0    = error "chunksOf: n must be 1 or more"
  | otherwise = go xs
  where
    go [] = []
    go ys = let (front, rest) = splitAt n ys in front : go rest

-- 🧩 Summarize each piece on its own, then combine the summaries.
--    Because <> is associative, this is the same as summarizing it all.
-- かたまりごとに別々にまとめてから、まとめどうしを合わせる。
-- <> が結合法則を守るので、全部を一度にまとめたのと同じになる
summarizeInChunks :: Int -> [Int] -> Summary
summarizeInChunks n times =
  mconcat [ foldMap summarize piece | piece <- chunksOf n times ]

-- ❌ What not to do: the mean of the means of the pieces
--    (times must not be empty).
-- やってはいけないこと：かたまりごとの平均の、平均（times は空でない）
meanOfMeans :: Int -> [Int] -> Double
meanOfMeans n times =
  let means = [ fromIntegral (sum piece) / fromIntegral (length piece)
              | piece <- chunksOf n times ]
  in  sum means / fromIntegral (length means)
-- ANCHOR_END: chunks

-- ANCHOR: visits
-- 🗺️ The towns a party has visited. Combining is a union: any order,
--    any number of times, the same result (a semilattice).
-- パーティーが訪れた町。合わせるのは和集合：どの順でも、何度でも、
-- 同じ結果（半束）
newtype Visits = Visits (Set String)
  deriving (Show, Eq)

instance Semigroup Visits where
  Visits a <> Visits b = Visits (Set.union a b)

instance Monoid Visits where
  mempty = Visits Set.empty

visit :: String -> Visits
visit town = Visits (Set.singleton town)
-- ANCHOR_END: visits
-- ANCHOR_END: all
