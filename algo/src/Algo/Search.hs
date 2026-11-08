-- ANCHOR: all
-- | Search: try everything with the list monad, and cut early with guard.
-- 探索：リストモナドで全部試し、guard で早めに枝を刈る
module Algo.Search
  ( Hero (..)
  , heroes
  , parties
  , queens
  , select
  , Puzzle (..)
  , sendMoreMoneySlow
  , sendMoreMoney
  ) where

-- ANCHOR: imports
import Control.Monad (guard)
import Control.Monad.State (StateT (..), evalStateT, lift)
import Data.List (tails)
-- ANCHOR_END: imports

-- ANCHOR: heroes
-- 🦸 A hero who can join: name, job and fee.
-- 仲間にできる勇者：名前、職業、報酬
data Hero = Hero { name :: String, job :: String, fee :: Int }
  deriving (Show, Eq)

heroes :: [Hero]
heroes =
  [ Hero "Lambda" "mage"    40, Hero "Python" "healer"  30
  , Hero "Rust"   "knight"  50, Hero "Ruby"   "healer"  25
  , Hero "TS"     "archer"  35, Hero "GHC"    "knight"  45 ]

-- 👥 Every party of three that fits the budget and has a healer.
--    Reads like the rules: pick three (in order, no repeats), then check.
-- 予算に収まり、回復役のいる3人組を全部。
-- 決まりのとおりに読める：3人選んで（順番どおり、重なりなく）、確かめる
parties :: Int -> [[String]]
parties budget = do
  (a : rest1) <- tails heroes
  (b : rest2) <- tails rest1
  c           <- rest2
  let team = [a, b, c]
  guard (sum (map fee team) <= budget)
  guard (any (\h -> job h == "healer") team)
  pure (map name team)
-- ANCHOR_END: heroes

-- ANCHOR: queens
-- 👑 N queens: one queen per row; qs holds the columns of the rows below.
--    A new queen must not share a column or a diagonal with any of them.
-- N クイーン：1行に1つ。qs は、下の行のクイーンの列。
-- 新しいクイーンは、列も斜めも、どのクイーンとも重ならない
queens :: Int -> [[Int]]
queens n = place n
  where
    place 0 = [[]]
    place k = do
      qs <- place (k - 1)
      q  <- [1 .. n]
      guard (safe q qs)
      pure (q : qs)
    safe q qs = and [ q /= c && abs (q - c) /= d | (d, c) <- zip [1 ..] qs ]
-- ANCHOR_END: queens

-- ANCHOR: select
-- 🎯 Pick one element, and keep the rest: every way.
-- 1つ選んで、残りを持っておく。すべての選び方で
select :: [a] -> [(a, [a])]
select []       = []
select (x : xs) = (x, xs) : [ (y, x : ys) | (y, ys) <- select xs ]
-- ANCHOR_END: select

-- ANCHOR: puzzle
-- 🔤 SEND + MORE = MONEY: a different digit for each letter.
-- SEND + MORE = MONEY：文字ごとに、ちがう数字
data Puzzle = Puzzle { send :: Int, more :: Int, money :: Int }
  deriving (Show, Eq)

-- 🔢 Pick an unused digit: StateT keeps the digits left, [] tries each.
-- まだ使っていない数字を選ぶ：StateT が残りの数字を持ち、[] が全部試す
digit :: StateT [Int] [] Int
digit = StateT select

number :: [Int] -> Int
number = foldl (\acc d -> acc * 10 + d) 0
-- ANCHOR_END: puzzle

-- ANCHOR: slow
-- 🐢 Try every assignment, check at the very end.
-- 全部の割り当てを試して、最後に確かめる
sendMoreMoneySlow :: [Puzzle]
sendMoreMoneySlow = flip evalStateT [0 .. 9] $ do
  s <- digit; e <- digit; n <- digit; d <- digit
  m <- digit; o <- digit; r <- digit; y <- digit
  let sendN  = number [s, e, n, d]
      moreN  = number [m, o, r, e]
      moneyN = number [m, o, n, e, y]
  lift (guard (s /= 0 && m /= 0 && sendN + moreN == moneyN))
  pure (Puzzle sendN moreN moneyN)
-- ANCHOR_END: slow

-- ANCHOR: fast
-- 🐇 Check each column as soon as its digits are known, from the right.
-- 右の桁から、その桁の数字がそろったらすぐに確かめる
sendMoreMoney :: [Puzzle]
sendMoreMoney = flip evalStateT [0 .. 9] $ do
  d <- digit; e <- digit; y <- digit
  check ((d + e) `mod` 10 == y)
  let c1 = (d + e) `div` 10
  n <- digit; r <- digit
  check ((n + r + c1) `mod` 10 == e)
  let c2 = (n + r + c1) `div` 10
  o <- digit
  check ((e + o + c2) `mod` 10 == n)
  let c3 = (e + o + c2) `div` 10
  s <- digit; m <- digit
  check (s /= 0 && m /= 0 && (s + m + c3) `mod` 10 == o)
  check ((s + m + c3) `div` 10 == m)
  pure (Puzzle (number [s, e, n, d]) (number [m, o, r, e])
               (number [m, o, n, e, y]))
  where
    check = lift . guard
-- ANCHOR_END: fast
-- ANCHOR_END: all
