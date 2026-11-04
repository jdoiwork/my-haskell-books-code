-- ANCHOR: all
-- | Induction and recursion: stairs, dungeon maps, and loops that stop.
-- 帰納法と再帰：階段、ダンジョンの地図、止まるループ
module Math.Induction
  ( sumTo
  , sumFormula
  , Room (..)
  , forks
  , deadEnds
  , power
  , powerDraft
  , collatzSteps
  , isPrime
  , eulerNumber
  ) where

-- ANCHOR: sum
-- 🪜 1 + 2 + ... + n, one stair at a time.
-- 1 から n までの和を、1段ずつ
sumTo :: Integer -> Integer
sumTo 0 = 0
sumTo n = n + sumTo (n - 1)

-- ⚡ The same, in one step: n (n + 1) / 2.
-- 同じものを、一発で
sumFormula :: Integer -> Integer
sumFormula n = n * (n + 1) `div` 2
-- ANCHOR_END: sum

-- ANCHOR: room
-- 🗺️ A dungeon map: every path ends in a dead end, or forks in two.
-- ダンジョンの地図：道は、行き止まりか、2つに分かれるか
data Room = DeadEnd | Fork Room Room
  deriving (Show)

-- 🔀 How many forks? / 分かれ道の数
forks :: Room -> Int
forks DeadEnd    = 0
forks (Fork l r) = 1 + forks l + forks r

-- 🧱 How many dead ends? / 行き止まりの数
deadEnds :: Room -> Int
deadEnds DeadEnd    = 1
deadEnds (Fork l r) = deadEnds l + deadEnds r
-- ANCHOR_END: room

-- ANCHOR: power
-- 💪 x ^ n by squaring. The loop keeps one promise (the invariant):
--      acc * b ^ e == x ^ n
--    and e gets smaller every turn, so the loop stops.
-- 2乗をくり返す、速いべき乗。ループは1つの約束（不変条件）を守る：
--   acc * b ^ e == x ^ n 。e は毎回小さくなるので、ループは止まる
power :: Integer -> Int -> Integer
power x n = go 1 x n
  where
    go acc b e
      | e == 0    = acc
      | even e    = go acc       (b * b) (e `div` 2)
      | otherwise = go (acc * b) b       (e - 1)

-- 🐛 A draft that squares even when e is odd.
-- 下書き：e が奇数のときも、2乗してしまう
powerDraft :: Integer -> Int -> Integer
powerDraft x n = go 1 x n
  where
    go acc b e
      | e == 0    = acc
      | otherwise = go acc (b * b) (e `div` 2)
-- ANCHOR_END: power

-- ANCHOR: collatz
-- 🌀 Halve it if even, else 3n + 1, until 1. Does it always stop?
--    Nobody knows (but it has stopped for every number tried).
-- 偶数なら半分、奇数なら 3n + 1。1 になるまで。いつも止まる？
-- 誰も知らない（試した数では、全部止まった）
collatzSteps :: Integer -> Int
collatzSteps 1 = 0
collatzSteps n
  | even n    = 1 + collatzSteps (n `div` 2)
  | otherwise = 1 + collatzSteps (3 * n + 1)
-- ANCHOR_END: collatz

-- ANCHOR: euler
-- 🔍 Is n a prime? (Try dividing by 2, 3, ... up to the square root.)
-- n は素数？（2, 3, ... と平方根まで割ってみる）
isPrime :: Integer -> Bool
isPrime n
  | n < 2     = False
  | otherwise = all (\d -> n `mod` d /= 0)
                    (takeWhile (\d -> d * d <= n) [2 ..])

-- 🎭 Euler's n^2 + n + 41: a prime for n = 0, 1, 2, ... (for a while).
-- オイラーの n^2 + n + 41：0, 1, 2, ... で素数になる（しばらくは）
eulerNumber :: Integer -> Integer
eulerNumber n = n * n + n + 41
-- ANCHOR_END: euler
-- ANCHOR_END: all
