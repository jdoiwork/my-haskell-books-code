{-# OPTIONS_GHC -Wno-orphans #-}
-- ANCHOR: all
-- | Answers to the training of Part VI.
-- 第6部のトレーニングの解答
module AlgoAnswers
  ( bestRunNonEmpty
  , bestRunWhere
  , makeChange
  , msortBy
  , lcsLength
  , gridPaths
  , islands
  , longestRun
  , depthAlg
  , collatzSteps
  , queueLength
  , peek
  , kSmallest
  ) where

import Control.Monad (guard)
import Data.Array
import Data.List (maximumBy, tails)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set

import Algo.Purely (Queue, heapSort, pop)
import Algo.Schemes (ExprF (..), ListF (..), hylo)
import Algo.Strings (Trie (..), runLength)

-- ANCHOR: nonEmpty
-- 🔁 "Thinking" step 2: at least one room must be visited.
--    The empty run is gone, so the answer can be negative.
-- 「仕様から始める」ステップ2：少なくとも1部屋は通る。
-- 空の並びがなくなるので、答えがマイナスになることもある
bestRunNonEmpty :: [Int] -> Int
bestRunNonEmpty xs = maximum (scanr1 step xs)
  where
    step x best = max x (x + best)
-- ANCHOR_END: nonEmpty

-- ANCHOR: where
-- 📍 "Thinking" step 3: also say where the best run starts and how many
--    rooms it has: (total, start, length). On a tie, the earliest start,
--    then the shortest run, wins. O(n^2), straight from the spec.
-- 「仕様から始める」ステップ3：いちばんよい並びが、どこから何部屋かも言う。
-- （合計, 始まり, 部屋の数）。同じ合計なら、早く始まるほう、短いほう。
-- 仕様からそのまま作るので O(n^2)
bestRunWhere :: [Int] -> (Int, Int, Int)
bestRunWhere xs = maximumBy (comparing key) candidates
  where
    candidates =
      [ (total, start, len)
      | (start, rest) <- zip [0 ..] (tails xs)
      , (len, total)  <- zip [0 ..] (scanl (+) 0 rest) ]
    key (total, start, len) = (total, Down start, Down len)
-- ANCHOR_END: where
-- ANCHOR: change
-- 🪙 "Search" step 3: every way to pay exactly, as a list of coins.
--    Coins are taken from the same kind or a later one, so no repeats.
-- 「探索」ステップ3：ちょうど払う方法を、硬貨のリストで全部。
-- 同じ種類か、後ろの種類からだけ取るので、重ならない
makeChange :: Int -> [Int] -> [[Int]]
makeChange 0 _ = [[]]
makeChange amount coins = do
  (c : rest) <- tails coins
  guard (c <= amount)
  more <- makeChange (amount - c) (c : rest)
  pure (c : more)
-- ANCHOR_END: change
-- ANCHOR: msortBy
-- 🔀 "Divide and conquer" step 2: merge sort with your own comparison.
--    On a tie the left one goes first, so equal elements keep their order.
-- 「分割統治」ステップ2：自分の比べ方でマージソート。
-- 同じなら左を先に出すので、等しいものの順番は保たれる（安定）
msortBy :: (a -> a -> Ordering) -> [a] -> [a]
msortBy _   []  = []
msortBy _   [x] = [x]
msortBy cmp xs  =
  let (left, right) = splitAt (length xs `div` 2) xs
  in  mergeBy (msortBy cmp left) (msortBy cmp right)
  where
    mergeBy [] ys = ys
    mergeBy as [] = as
    mergeBy (a : as) (b : bs)
      | cmp a b /= GT = a : mergeBy as (b : bs)
      | otherwise     = b : mergeBy (a : as) bs
-- ANCHOR_END: msortBy
-- ANCHOR: lcs
-- 🔗 "Dynamic programming" step 2: the length of the longest common
--    subsequence (letters in the same order, not necessarily together).
-- 「動的計画法」ステップ2：最長共通部分列の長さ
-- （同じ順番に出てくる文字。となり合っていなくてよい）
lcsLength :: String -> String -> Int
lcsLength s t = l 0 0
  where
    m  = length s
    n  = length t
    sa = listArray (0, m - 1) s
    ta = listArray (0, n - 1) t
    table = listArray ((0, 0), (m, n))
              [ go i j | i <- [0 .. m], j <- [0 .. n] ]
    l i j = table ! (i, j)
    go i j
      | i == m || j == n = 0
      | sa ! i == ta ! j = 1 + l (i + 1) (j + 1)
      | otherwise        = max (l (i + 1) j) (l i (j + 1))
-- ANCHOR_END: lcs

-- ANCHOR: grid
-- 🗺️ "Dynamic programming" step 3: paths from the top-left to the
--    bottom-right, moving right or down, never stepping on a trap ('#').
-- 「動的計画法」ステップ3：左上から右下へ、右か下に進み、
-- 罠（'#'）を踏まない道の数
gridPaths :: [String] -> Integer
gridPaths rows = paths (h - 1) (w - 1)
  where
    h = length rows
    w = length (head' rows)
    head' (r : _) = r
    head' []      = []
    cell  = listArray ((0, 0), (h - 1, w - 1)) (concat rows)
    table = listArray ((0, 0), (h - 1, w - 1))
              [ go r c | r <- [0 .. h - 1], c <- [0 .. w - 1] ]
    paths r c
      | r < 0 || c < 0 = 0
      | otherwise      = table ! (r, c)
    go r c
      | cell ! (r, c) == '#' = 0
      | r == 0 && c == 0     = 1
      | otherwise            = paths (r - 1) c + paths r (c - 1)
-- ANCHOR_END: grid
-- ANCHOR: islands
-- 🏝️ "Graphs" step 2: how many islands ('#' cells joined up, down, left
--    or right) are on the map? Flood one island at a time.
-- 「グラフ」ステップ2：地図に島（上下左右につながった '#' のマス）は
-- いくつ？ 島を1つずつ塗りつぶす
islands :: [String] -> Int
islands rows = go land 0
  where
    land = Set.fromList [ (r, c) | (r, row) <- zip [0 :: Int ..] rows
                                 , (c, ch) <- zip [0 :: Int ..] row
                                 , ch == '#' ]
    go left n = case Set.lookupMin left of
      Nothing   -> n
      Just cell ->
        go (left `Set.difference` flood (Set.singleton cell)) (n + 1)
    flood seen =
      let next = Set.fromList [ (r + dr, c + dc)
                              | (r, c) <- Set.toList seen
                              , (dr, dc) <- [(1, 0), (-1, 0), (0, 1), (0, -1)]
                              , (r + dr, c + dc) `Set.member` land ]
                   `Set.union` seen
      in  if next == seen then seen else flood next
-- ANCHOR_END: islands
-- ANCHOR: longestRun
-- 🧱 "Strings" step 2: the longest run of one letter (the first, on a tie).
-- 「文字列」ステップ2：同じ文字のいちばん長い並び（同じ長さなら先のもの）
longestRun :: String -> Maybe (Char, Int)
longestRun s = case runLength s of
  []   -> Nothing
  runs -> Just (foldr1 longer runs)
  where
    longer a b = if snd b > snd a then b else a
-- ANCHOR_END: longestRun

-- ANCHOR: trieMerge
-- 🤝 "Strings" step 3: two tries merge into one, letter by letter.
-- 「文字列」ステップ3：2つのトライを、文字ごとに1つにまとめる
instance Semigroup Trie where
  Trie e1 c1 <> Trie e2 c2 = Trie (e1 || e2) (Map.unionWith (<>) c1 c2)

instance Monoid Trie where
  mempty = Trie False Map.empty
-- ANCHOR_END: trieMerge
-- ANCHOR: depth
-- 📏 "Recursion schemes" step 2: how deep is the expression tree?
--    The children are already depths; add one for this level.
-- 「再帰スキーム」ステップ2：式の木の深さ。
-- 子どもは、もう深さになっている。この段の分の 1 を足す
depthAlg :: ExprF Int -> Int
depthAlg (LitF _)   = 1
depthAlg (AddF a b) = 1 + max a b
depthAlg (MulF a b) = 1 + max a b
-- ANCHOR_END: depth

-- ANCHOR: collatz
-- 🔢 "Recursion schemes" step 3: steps of the Collatz sequence down to 1,
--    as a hylo: unfold the sequence, fold by counting.
-- 「再帰スキーム」ステップ3：コラッツの数列が 1 になるまでの歩数を hylo で。
-- 数列を広げて、数えて畳む
collatzSteps :: Int -> Int
collatzSteps = hylo count next
  where
    next 1 = NilF
    next n
      | even n    = ConsF n (n `div` 2)
      | otherwise = ConsF n (3 * n + 1)
    count NilF        = 0
    count (ConsF _ k) = 1 + k
-- ANCHOR_END: collatz

-- ANCHOR: queueLength
-- 📏 "Purely" step 2: how many are in line, and who is first.
--    Use only pop, so the answer does not depend on the inside.
-- 「純粋なデータ構造」ステップ2：何人並んでいるか、先頭はだれか。
-- pop だけを使うので、中身の作りに左右されない
queueLength :: Queue a -> Int
queueLength q = case pop q of
  Nothing      -> 0
  Just (_, q') -> 1 + queueLength q'

peek :: Queue a -> Maybe a
peek q = fst <$> pop q
-- ANCHOR_END: queueLength

-- ANCHOR: kSmallest
-- 🥉 "Purely" step 3: the k smallest values.
--    heapSort is lazy, so take stops after k pops.
-- 「純粋なデータ構造」ステップ3：小さいほうから k 個。
-- heapSort は遅延なので、take は k 回取り出したところで止まる
kSmallest :: Ord a => Int -> [a] -> [a]
kSmallest k xs = take k (heapSort xs)
-- ANCHOR_END: kSmallest
-- ANCHOR_END: all
