-- ANCHOR: all
-- | Dynamic programming: remember the answers to small problems.
--   A lazy array can be defined in terms of itself, so the table fills
--   itself in, in whatever order the answers are needed.
-- 動的計画法：小さな問題の答えを覚えておく。遅延な配列は、自分自身を
-- 使って定義できるので、表は、必要になった順に、ひとりでに埋まる
module Algo.Dp
  ( fibs
  , waysSpec
  , ways
  , editDistanceSpec
  , editDistance
  , Item (..)
  , treasures
  , knapsack
  ) where

-- ANCHOR: imports
import Data.Array
-- ANCHOR_END: imports

-- ANCHOR: fibs
-- 🐇 Every Fibonacci number, defined from the list itself.
-- フィボナッチ数を全部。リスト自身を使って定義する
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)
-- ANCHOR_END: fibs

-- ANCHOR: waysSpec
-- 📜 The spec: ways to pay an amount with the first k kinds of coins.
--    Either use no coin of kind k, or use one and pay the rest. Slow:
--    the same small problems are solved again and again.
-- 仕様：最初の k 種類の硬貨で払う方法の数。k 番目の硬貨を使わないか、
-- 1枚使って残りを払うか。遅い：同じ小さな問題を、何度も解き直す
waysSpec :: [Int] -> Int -> Integer
waysSpec coins amount = go (length coins) amount
  where
    go _ 0 = 1
    go 0 _ = 0
    go k a
      | a < 0     = 0
      | otherwise = go (k - 1) a + go k (a - coins !! (k - 1))
-- ANCHOR_END: waysSpec

-- ANCHOR: ways
-- 📋 The same recursion, but go looks the answers up in a table, and the
--    table is filled with go. Laziness ties the knot.
-- 同じ再帰。ただし go は答えを表から引き、表は go で埋める。
-- 遅延評価が、この輪を結んでくれる
ways :: [Int] -> Int -> Integer
ways coins amount = table ! (n, amount)
  where
    n     = length coins
    coin  = listArray (1, n) coins
    area  = ((0, 0), (n, amount))
    table = listArray area [ go k a | (k, a) <- range area ]
    look k a
      | a < 0     = 0
      | otherwise = table ! (k, a)
    go _ 0 = 1
    go 0 _ = 0
    go k a = look (k - 1) a + look k (a - coin ! k)
-- ANCHOR_END: ways

-- ANCHOR: editSpec
-- 📜 The spec of the edit distance: how many one-letter changes (insert,
--    delete, replace) turn one name into the other.
-- 編集距離の仕様：1文字の変更（足す・消す・置き換える）を何回すれば、
-- 名前がもう一方になるか
editDistanceSpec :: String -> String -> Int
editDistanceSpec [] ys = length ys
editDistanceSpec xs [] = length xs
editDistanceSpec (x : xs) (y : ys)
  | x == y    = editDistanceSpec xs ys
  | otherwise = 1 + minimum [ editDistanceSpec xs (y : ys)
                            , editDistanceSpec (x : xs) ys
                            , editDistanceSpec xs ys ]
-- ANCHOR_END: editSpec

-- ANCHOR: edit
-- 📋 The same, on suffixes numbered by where they start: d i j is the
--    distance between the rests of the names from positions i and j.
-- 同じことを、始まる位置で番号を付けた後ろ半分で。d i j は、
-- 位置 i と j から後ろの名前どうしの距離
editDistance :: String -> String -> Int
editDistance s t = d 0 0
  where
    m  = length s
    n  = length t
    sa = listArray (0, m - 1) s
    ta = listArray (0, n - 1) t
    table = listArray ((0, 0), (m, n))
              [ dist i j | i <- [0 .. m], j <- [0 .. n] ]
    d i j = table ! (i, j)
    dist i j
      | i == m             = n - j
      | j == n             = m - i
      | sa ! i == ta ! j   = d (i + 1) (j + 1)
      | otherwise          = 1 + minimum [ d (i + 1) j
                                         , d i (j + 1)
                                         , d (i + 1) (j + 1) ]
-- ANCHOR_END: edit

-- ANCHOR: knapsack
-- 💎 A treasure: its weight and its value.
-- 宝：重さと価値
data Item = Item { itemName :: String, weight :: Int, value :: Int }
  deriving (Show, Eq)

treasures :: [Item]
treasures =
  [ Item "crown" 5 60, Item "sword" 4 40, Item "gem" 1 30
  , Item "shield" 6 45, Item "potion" 2 20 ]

-- 🎒 The best value that fits in a bag of capacity w, using the first k
--    treasures: leave treasure k, or take it if it fits.
-- 最初の k 個の宝で、容量 w の袋に入る最大の価値：
-- k 番目を置いていくか、入るなら持っていくか
knapsack :: [Item] -> Int -> Int
knapsack items capacity = best (length items) capacity
  where
    n     = length items
    item  = listArray (1, n) items
    table = listArray ((0, 0), (n, capacity))
              [ choose k w | k <- [0 .. n], w <- [0 .. capacity] ]
    best k w = table ! (k, w)
    choose 0 _ = 0
    choose k w
      | weight it > w = leave
      | otherwise     = max leave (value it + best (k - 1) (w - weight it))
      where
        it    = item ! k
        leave = best (k - 1) w
-- ANCHOR_END: knapsack
-- ANCHOR_END: all
