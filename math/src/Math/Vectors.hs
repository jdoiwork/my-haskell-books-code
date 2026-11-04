-- ANCHOR: all
-- | Vectors and matrices: many numbers, moved together.
-- ベクトルと行列：たくさんの数を、まとめて動かす
module Math.Vectors
  ( Vector
  , Matrix
  , add
  , scale
  , dot
  , norm
  , cosine
  , monsters
  , mostSimilar
  , mmul
  , apply
  , identity
  , rotate90
  , stretchX
  ) where

import Data.List (maximumBy, transpose)
import Data.Ord (comparing)

-- ANCHOR: vector
-- ➡️ A vector: a list of numbers, like [attack, defense, speed, magic].
-- ベクトル：数のリスト。たとえば [攻撃, 防御, 素早さ, 魔力]
type Vector a = [a]

-- ➕ Add two vectors, number by number.
-- 2つのベクトルを、数ごとに足す
add :: Num a => Vector a -> Vector a -> Vector a
add = zipWith (+)

-- ✖️ Multiply every number by k.
-- すべての数を k 倍する
scale :: Num a => a -> Vector a -> Vector a
scale k = map (k *)
-- ANCHOR_END: vector

-- ANCHOR: dot
-- 🎯 The dot product: multiply number by number, then add up.
-- 内積：数ごとに掛けて、全部足す
dot :: Num a => Vector a -> Vector a -> a
dot u v = sum (zipWith (*) u v)

-- 📏 The length of a vector (Pythagoras): the square root of v . v.
-- ベクトルの長さ（三平方の定理）：v と v の内積の平方根
norm :: Vector Double -> Double
norm v = sqrt (dot v v)

-- 🧭 How much two vectors point the same way: 1 = same direction,
--    0 = unrelated (at a right angle). The lengths do not matter.
-- 2つのベクトルが、どれだけ同じ向きか：1 は同じ向き、0 は無関係（直角）。
-- 長さは関係ない
cosine :: Vector Double -> Vector Double -> Double
cosine u v = dot u v / (norm u * norm v)
-- ANCHOR_END: dot

-- ANCHOR: monsters
-- 👾 Monsters as vectors: [attack, defense, speed, magic].
-- モンスターをベクトルで：[攻撃, 防御, 素早さ, 魔力]
monsters :: [(String, Vector Double)]
monsters =
  [ ("Slime",       [ 2,  2,  3,  1])
  , ("Goblin",      [ 6,  3,  6,  1])
  , ("Golem",       [ 8, 12,  1,  1])
  , ("Wizard",      [ 2,  3,  4, 12])
  , ("Dragon",      [30, 25, 15, 20])
  , ("Fairy",       [ 1,  2,  9,  8])
  ]

-- 🔍 The monster most like this one (by cosine), other than itself.
-- いちばん似ているモンスター（コサインで比べる）。自分は除く
mostSimilar :: String -> Maybe String
mostSimilar name = do
  me <- lookup name monsters
  let others = [ m | m@(n, _) <- monsters, n /= name ]
  if null others
    then Nothing
    else Just (fst (maximumBy (comparing (cosine me . snd)) others))
-- ANCHOR_END: monsters

-- ANCHOR: matrix
-- 🔲 A matrix: a list of rows.
-- 行列：行のリスト
type Matrix a = [[a]]

-- ✖️ Matrix times matrix: row by column, dot products.
-- 行列の積：行と列の内積を並べる
mmul :: Num a => Matrix a -> Matrix a -> Matrix a
mmul a b = [ [ dot row col | col <- transpose b ] | row <- a ]

-- ➡️ Matrix times vector: move a point.
-- 行列 × ベクトル：点を動かす
apply :: Num a => Matrix a -> Vector a -> Vector a
apply m v = [ dot row v | row <- m ]

-- 🟰 The matrix that changes nothing.
-- 何も変えない行列
identity :: Num a => Int -> Matrix a
identity n = [ [ if i == j then 1 else 0 | j <- [1 .. n] ] | i <- [1 .. n] ]

-- 🔄 Turn a point a quarter to the left, around (0, 0).
-- 点を (0, 0) のまわりに、左へ 90 度回す
rotate90 :: Num a => Matrix a
rotate90 = [ [0, -1]
           , [1,  0] ]

-- ↔️ Stretch sideways: x becomes 2x, y stays.
-- 横に伸ばす：x は 2 倍、y はそのまま
stretchX :: Num a => Matrix a
stretchX = [ [2, 0]
           , [0, 1] ]
-- ANCHOR_END: matrix
-- ANCHOR_END: all
