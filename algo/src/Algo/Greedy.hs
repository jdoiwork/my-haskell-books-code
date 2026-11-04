-- ANCHOR: all
-- | Greedy algorithms: take the best-looking choice now, and never look
--   back. Sometimes right, sometimes not; QuickCheck tells which.
-- 貪欲法：今いちばんよさそうなものを取り、ふり返らない。
-- 正しいときも、まちがいのときもある。どちらかは QuickCheck が教えてくれる
module Algo.Greedy
  ( Quest (..)
  , quests
  , overlaps
  , mostQuestsSpec
  , mostQuests
  , mostQuestsBy
  , fewestCoinsSpec
  , greedyCoins
  , Huffman (..)
  , huffman
  , codes
  , encode
  , decode
  ) where

import Data.List (sortOn, subsequences)
import qualified Data.Map.Strict as Map

-- ANCHOR: quest
-- 📅 A quest takes the hours from start to end (end not included).
-- 依頼は、start 時から end 時まで（end は含まない）
data Quest = Quest { title :: String, start :: Int, end :: Int }
  deriving (Show, Eq)

quests :: [Quest]
quests =
  [ Quest "dragon"  1 9, Quest "herbs"  2 4, Quest "goblins" 4 6
  , Quest "escort"  5 8, Quest "slimes" 6 7, Quest "letter"  8 10 ]

overlaps :: Quest -> Quest -> Bool
overlaps a b = start a < end b && start b < end a

-- 📜 The spec: try every set of quests, keep those with no overlap,
--    and take the biggest.
-- 仕様：依頼の組み合わせを全部試し、重ならないものの中で、いちばん多いもの
mostQuestsSpec :: [Quest] -> Int
mostQuestsSpec qs =
  maximum [ length pick | pick <- subsequences qs, noOverlap pick ]
  where
    noOverlap pick =
      and [ not (overlaps a b) | (i, a) <- zip [0 :: Int ..] pick
                               , (j, b) <- zip [0 ..] pick, i < j ]
-- ANCHOR_END: quest

-- ANCHOR: greedy
-- 🏃 Greedy: sort by end, and take each quest that does not overlap
--    the ones already taken.
-- 貪欲：終わる順に並べ、もう取った依頼と重ならないものを取っていく
mostQuests :: [Quest] -> [Quest]
mostQuests = mostQuestsBy end

-- 🎛️ The same, with any order. Only "by end" is always right.
-- 同じことを、好きな順番で。いつも正しいのは「終わる順」だけ
mostQuestsBy :: (Quest -> Int) -> [Quest] -> [Quest]
mostQuestsBy key qs = reverse (foldl takeIfFree [] (sortOn key qs))
  where
    takeIfFree taken q
      | any (overlaps q) taken = taken
      | otherwise              = q : taken
-- ANCHOR_END: greedy

-- ANCHOR: coins
-- 📜 The spec: the fewest coins that pay exactly (Nothing if impossible).
-- 仕様：ちょうど払うのに要る、いちばん少ない硬貨の枚数
-- （払えなければ Nothing）
fewestCoinsSpec :: [Int] -> Int -> Maybe Int
fewestCoinsSpec coins amount = go amount
  where
    go 0 = Just 0
    go a = minimum' [ (+ 1) <$> go (a - c) | c <- coins, c <= a ]
    minimum' xs = case [ x | Just x <- xs ] of
      [] -> Nothing
      ys -> Just (minimum ys)

-- 🪙 Greedy: always the biggest coin that still fits.
-- 貪欲：入るいちばん大きな硬貨から使う
greedyCoins :: [Int] -> Int -> Maybe Int
greedyCoins coins amount = go (reverse (sortOn id coins)) amount 0
  where
    go _ 0 n = Just n
    go [] _ _ = Nothing
    go (c : cs) a n
      | c <= a    = go (c : cs) (a - c) (n + 1)
      | otherwise = go cs a n
-- ANCHOR_END: coins

-- ANCHOR: huffman
-- 🌳 A Huffman tree: letters at the leaves; left is 0, right is 1.
-- ハフマンの木：葉に文字。左が 0、右が 1
data Huffman = Leaf Char | Node Huffman Huffman
  deriving (Show, Eq)

-- 🏗️ Repeatedly take the two lightest trees and join them. The Map,
--    keyed by (weight, number), is our priority queue.
-- いちばん軽い2本の木を取り出して、つなぐ、をくり返す。
-- （重さ, 番号）を鍵にした Map が、優先度つきキューの代わり
huffman :: String -> Huffman
huffman text = build (length start') queue
  where
    counts = Map.toList (Map.fromListWith (+) [ (c, 1 :: Int) | c <- text ])
    start' = [ (w, Leaf c) | (c, w) <- counts ]
    queue  = Map.fromList [ ((w, i), t) | (i, (w, t)) <- zip [0 ..] start' ]
    build next q = case Map.minViewWithKey q of
      Nothing -> error "huffman: empty text"
      Just (((w1, _), t1), rest) -> case Map.minViewWithKey rest of
        Nothing -> t1
        Just (((w2, _), t2), rest') ->
          build (next + 1)
                (Map.insert (w1 + w2, next) (Node t1 t2) rest')
-- ANCHOR_END: huffman

-- ANCHOR: code
-- 🔤 The code of each letter: the path from the root to its leaf.
-- 文字ごとの符号：根から葉までの道順
codes :: Huffman -> Map.Map Char String
codes (Leaf c) = Map.singleton c "0"
codes tree     = Map.fromList (walk tree "")
  where
    walk (Leaf c) path   = [(c, reverse path)]
    walk (Node l r) path = walk l ('0' : path) ++ walk r ('1' : path)

encode :: Huffman -> String -> String
encode tree text = concatMap (codes tree Map.!) text

-- 🔓 Follow the bits from the root; at a leaf, write its letter and start
--    again from the root.
-- 根からビットをたどる。葉に着いたら文字を書き、また根から
decode :: Huffman -> String -> String
decode (Leaf c) bits = map (const c) bits
decode tree bits = go tree bits
  where
    go (Leaf c) rest          = c : go tree rest
    go _ []                   = []
    go (Node l _) ('0' : rest) = go l rest
    go (Node _ r) (_ : rest)   = go r rest
-- ANCHOR_END: code
-- ANCHOR_END: all
