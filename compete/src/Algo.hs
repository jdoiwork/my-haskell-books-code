-- ANCHOR: all
-- ANCHOR: module
-- 🧠 The algorithms of this chapter. Each one is a pure function.
-- この章のアルゴリズム。どれも純粋な関数
module Algo
  ( prefixSums
  , rangeSum
  , rangeSumNaive
  , turnsNeeded
  , weakestAttack
  , weakestAttackNaive
  , bestBag
  , shortestPath
  , twoSum
  ) where
-- ANCHOR_END: module

-- ANCHOR: imports
import Control.Monad.ST            (ST, runST)
import Data.List                   qualified as L
import Data.Map.Strict             qualified as Map
import Data.Sequence               (Seq (..), (|>))
import Data.Sequence               qualified as Seq
import Data.Vector.Unboxed         qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU
-- ANCHOR_END: imports

-- ANCHOR: prefix
-- ➕ Prefix sums: sums !! i is the total of the first i damages.
-- 累積和：i 番目は、最初の i 個のダメージの合計
--   [3, 1, 4, 1, 5] -> [0, 3, 4, 8, 9, 14]
prefixSums :: [Int] -> VU.Vector Int
prefixSums damages = VU.fromList (scanl (+) 0 damages)

-- 📏 The total from the l-th to the r-th damage (1-based, both ends).
--    Two lookups, whatever the length: O(1).
-- l 番目から r 番目までの合計（1 から数え、両端を含む）。
-- 長さに関係なく、2回引くだけ：O(1)
rangeSum :: VU.Vector Int -> Int -> Int -> Int
rangeSum sums l r = sums VU.! r - sums VU.! (l - 1)

-- 🐢 The same, by adding them up each time: O(r)
--    (drop walks the first l - 1 items too).
-- 同じものを、毎回足して：O(r)（drop が、最初の l - 1 個もたどる）
rangeSumNaive :: [Int] -> Int -> Int -> Int
rangeSumNaive damages l r = sum (take (r - l + 1) (drop (l - 1) damages))
-- ANCHOR_END: prefix

-- ANCHOR: binary
-- ⏳ Turns to beat every monster with this attack, one at a time.
-- この攻撃力で、モンスターを1体ずつ全部倒すのにかかるターン数
turnsNeeded :: [Int] -> Int -> Int
turnsNeeded hps attack = sum [ (hp + attack - 1) `div` attack | hp <- hps ]

-- 🎯 The weakest attack that beats them all in k turns or fewer
--    (k must be at least the number of monsters). A stronger attack
--    never needs more turns, so we can halve the range each time:
--    O(n log m).
-- k ターン以内に全部倒せる、いちばん弱い攻撃力（k は、モンスターの数以上）。
-- 攻撃力が上がれば、ターン数は増えないので、
-- 範囲を毎回半分にできる：O(n log m)
weakestAttack :: [Int] -> Int -> Int
weakestAttack hps k = go 0 (maximum hps)
  where
    -- go lo hi: lo is not enough, hi is enough.
    -- lo では足りず、hi なら足りる
    go lo hi
      | hi - lo <= 1           = hi
      | turnsNeeded hps mid <= k = go lo mid
      | otherwise              = go mid hi
      where
        mid = (lo + hi) `div` 2

-- 🐢 The same, by trying 1, 2, 3, ... in order: O(n m).
-- 同じものを、1, 2, 3, ... と順に試して：O(n m)
weakestAttackNaive :: [Int] -> Int -> Int
weakestAttackNaive hps k =
  until (\a -> turnsNeeded hps a <= k) (+ 1) 1
-- ANCHOR_END: binary

-- ANCHOR: dp
-- 🎒 The most value we can carry, with this much room in the bag.
--    best ! w is the most value with room w, using the items so far.
-- 袋に入る重さの上限で、運べるいちばん大きな価値。
-- best ! w は、ここまでの宝物で、重さ w までに入る価値の最大
bestBag :: Int -> [(Int, Int)] -> Int
bestBag capacity items = VU.last (L.foldl' addItem start items)
  where
    start = VU.replicate (capacity + 1) 0
    addItem best (weight, value) = VU.generate (capacity + 1) pick
      where
        pick w
          | w < weight = best VU.! w
          | otherwise  = max (best VU.! w)
                             (best VU.! (w - weight) + value)
-- ANCHOR_END: dp

-- ANCHOR: bfs
-- 🗺️ The fewest steps from (0, 0) to the bottom-right corner.
--    '#' is a wall. Nothing if there is no way.
-- 左上 (0, 0) から右下までの、いちばん少ない歩数。
-- '#' は壁。たどり着けなければ Nothing
shortestPath :: [String] -> Maybe Int
shortestPath rows = runST $ do
  dist <- MVU.replicate (h * w) (-1)
  MVU.write dist 0 0
  bfs dist (Seq.singleton (0, 0))
  d <- MVU.read dist (h * w - 1)
  pure (if d < 0 then Nothing else Just d)
  where
    h    = length rows
    w    = case rows of
             (row : _) -> length row
             []        -> 0
    grid = VU.fromList (concat rows)
    open (r, c) = r >= 0 && r < h && c >= 0 && c < w
                  && grid VU.! (r * w + c) /= '#'

    bfs :: MVU.MVector s Int -> Seq (Int, Int) -> ST s ()
    bfs _    Empty                = pure ()
    bfs dist ((r, c) :<| queue) = do
      d <- MVU.read dist (r * w + c)
      let nexts = [ (r + dr, c + dc)
                  | (dr, dc) <- [(1, 0), (-1, 0), (0, 1), (0, -1)]
                  , open (r + dr, c + dc) ]
      fresh <- filterUnseen dist nexts
      mapM_ (\(nr, nc) -> MVU.write dist (nr * w + nc) (d + 1)) fresh
      bfs dist (L.foldl' (|>) queue fresh)

    filterUnseen dist ps = do
      seen <- mapM (\(r, c) -> MVU.read dist (r * w + c)) ps
      pure [ p | (p, s) <- zip ps seen, s < 0 ]
-- ANCHOR_END: bfs

-- ANCHOR: twosum
-- 🤝 Two positions whose numbers add up to the target (LeetCode 1).
--    Remember each number's position in a Map as we go: O(n log n).
-- 足すと target になる2つの位置（LeetCode の1問目）。
-- 見た数とその位置を Map に覚えながら進む：O(n log n)
twoSum :: [Int] -> Int -> Maybe (Int, Int)
twoSum nums target = go Map.empty (zip [0 ..] nums)
  where
    go _ [] = Nothing
    go seen ((i, x) : rest) =
      case Map.lookup (target - x) seen of
        Just j  -> Just (j, i)
        Nothing -> go (Map.insert x i seen) rest
-- ANCHOR_END: twosum
-- ANCHOR_END: all
