-- ANCHOR: all
-- ANCHOR: head
-- | The things to race, written in different ways.
-- 比べっこをするもの。同じ計算を、いくつかの書き方で
module Bench.Kernels
  ( randomMatrix
  , naiveMatmul
  , softmaxRowsNp
  , softmaxRowsV
  , softmaxRowsMassiv
  , checksum
  ) where

import qualified Data.Massiv.Array as A
import Data.Massiv.Array (Comp, Ix2 (..), Sz (..), (!), (!>))
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
import MiniTorch.Random (uniforms)
-- ANCHOR_END: head

-- ANCHOR: random
-- 🎲 A matrix of random numbers in [0, 1), from a seed.
-- 種から作る、[0, 1) のでたらめな数の行列
randomMatrix :: Word64 -> Int -> Int -> NDArray
randomMatrix seed r c =
  Np.fromVector [r, c] (VU.fromListN (r * c) (uniforms seed))
-- ANCHOR_END: random

-- ANCHOR: checksum
-- 🧾 One number from a whole array: the sum. The stopwatch makes
--    every program compute this, so nobody can skip work.
-- 配列全体から1つの数を作る（合計）。どのプログラムにもこれを計算させて、
-- 仕事を省けないようにする
checksum :: NDArray -> Double
checksum = Np.item . Np.sum
-- ANCHOR_END: checksum

-- ANCHOR: naive
-- 🐢 The matrix product with three loops, like the textbook:
--    c[i][j] = a[i][0] * b[0][j] + a[i][1] * b[1][j] + ...
--    The same loops as the Python, Julia and R versions.
-- 教科書どおり、3重のループで行列の積。Python・Julia・R の版と同じループ
naiveMatmul :: NDArray -> NDArray -> NDArray
naiveMatmul a b = case (Np.shape a, Np.shape b) of
  ([r, k], [k', c]) | k == k' ->
    let xs = Np.values a
        ys = Np.values b
        cell ij =
          let (i, j) = ij `divMod` c
              go !t !acc
                | t == k    = acc
                | otherwise =
                    let x = xs VU.! (i * k + t)
                        y = ys VU.! (t * c + j)
                    in go (t + 1) (acc + x * y)
          in go 0 0
    in Np.fromVector [r, c] (VU.generate (r * c) cell)
  (s1, s2) -> error ("naiveMatmul: " <> show s1 <> " and " <> show s2)
-- ANCHOR_END: naive

-- ANCHOR: softmaxNp
-- 🧮 Softmax of each row, with MiniNumpy (the answer of step 3 in the
--    NumPy chapter). Every step makes a new array, like NumPy.
-- 行ごとの softmax を MiniNumpy で（NumPy 風の配列の章のステップ3の解答）。
-- NumPy と同じく、1歩ごとに新しい配列ができる
softmaxRowsNp :: NDArray -> NDArray
softmaxRowsNp x =
  let rows   = Np.nrows x
      maxes  = Np.reshape [rows, 1]
                 (Np.array [ Np.maxAll (Np.row i x) | i <- [0 .. rows - 1] ])
      e      = exp (x - maxes)
      totals = Np.reshape [rows, 1] (Np.sumAxis 1 e)
  in e / totals
-- ANCHOR_END: softmaxNp

-- ANCHOR: softmaxV
-- ⚡ The same, one row at a time, straight on the vector. Inside a row,
--    VU.map fuses "subtract the max" and "exp" into one loop.
-- 同じ計算を、1行ずつ、Vector の上で。行の中では、VU.map が
-- 「最大値を引く」と「exp」を1つのループにまとめる（融合する）
softmaxRowsV :: NDArray -> NDArray
softmaxRowsV x = case Np.shape x of
  [r, c] ->
    let row i = VU.slice (i * c) c (Np.values x)
        one i =
          let v = row i
              m = VU.maximum v
              e = VU.map (\t -> exp (t - m)) v
              s = VU.sum e
          in VU.map (/ s) e
    in Np.fromVector [r, c] (VU.concat [ one i | i <- [0 .. r - 1] ])
  s -> error ("softmaxRowsV: not 2-D: " <> show s)
-- ANCHOR_END: softmaxV

-- ANCHOR: softmaxMassiv
-- 🧵 The same with massiv. Comp says how to compute: A.Seq on one core,
--    A.Par on all of them. The rows are split among the cores.
-- 同じ計算を massiv で。Comp が計算のしかたを決める：A.Seq は1コア、
-- A.Par は全部のコア。行を、コアで分けあう
softmaxRowsMassiv :: Comp -> NDArray -> NDArray
softmaxRowsMassiv comp x = case Np.shape x of
  [r, c] ->
    let -- Seq here: one row is worked on by one core. Only the outer
        -- makeArray below splits the rows among the cores (with Par,
        -- each row would be split again, and the cores would trip over
        -- each other).
        -- ここは Seq：1つの行は1つのコアが受け持つ。行をコアで分けるのは、
        -- 下の外側の makeArray だけ（Par にすると、行の中までまた分けて、
        -- コアどうしが足を引っぱりあう）
        arr :: A.Array A.U Ix2 Double
        arr   = A.resize' (Sz (r :. c))
                  (A.fromUnboxedVector A.Seq (Np.values x))
        maxes :: A.Array A.U A.Ix1 Double
        maxes = A.makeArray comp (Sz r) (\i -> A.maximum' (arr !> i))
        sums :: A.Array A.U A.Ix1 Double
        sums  = A.makeArray comp (Sz r) (\i ->
                  A.sum (A.map (\t -> exp (t - (maxes ! i))) (arr !> i)))
        out :: A.Array A.U Ix2 Double
        out   = A.makeArray comp (Sz (r :. c)) (\(i :. j) ->
                  exp ((arr ! (i :. j)) - (maxes ! i)) / (sums ! i))
    -- Compute maxes, then sums, before out. Otherwise the first core that
    -- needs maxes would compute it inside out's parallel work.
    -- out の前に、maxes、sums の順に計算させる。しないと、out を並列に
    -- 計算している最中に、最初に maxes が要ったコアがそれを計算することになる
    in maxes `seq` sums `seq` Np.fromVector [r, c] (A.toUnboxedVector out)
  s -> error ("softmaxRowsMassiv: not 2-D: " <> show s)
-- ANCHOR_END: softmaxMassiv
-- ANCHOR_END: all
