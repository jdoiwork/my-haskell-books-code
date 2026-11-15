-- ANCHOR: all
-- | Hand the matrix product to BLAS, through hmatrix.
-- 行列の積を、hmatrix を通して BLAS に任せる
module MiniNumpy.Blas
  ( toMatrix
  , fromMatrix
  , dot
  ) where

import qualified Data.Vector.Unboxed as VU
import qualified Numeric.LinearAlgebra as LA

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)

-- ANCHOR: convert
-- 🚪 A 2-D NDArray as an hmatrix matrix. Both keep the numbers
--    row by row, so only the kind of vector changes (Unboxed to Storable).
-- 2次元の NDArray を hmatrix の行列に。どちらも数を行の順に並べるので、
-- 変わるのはベクトルの種類だけ（Unboxed から Storable へ）
toMatrix :: NDArray -> LA.Matrix Double
toMatrix a = case Np.shape a of
  [_, c] -> LA.reshape c (VU.convert (Np.values a))
  s      -> error ("toMatrix: not 2-D: " <> show s)

-- 🚪 And back. / 逆向き
fromMatrix :: LA.Matrix Double -> NDArray
fromMatrix m =
  Np.fromVector [LA.rows m, LA.cols m] (VU.convert (LA.flatten m))
-- ANCHOR_END: convert

-- ANCHOR: dot
-- ✖️ Like Np.dot, but a matrix times a matrix goes to BLAS.
--    Other shapes use Np.dot as before.
-- Np.dot と同じ。ただし、行列×行列だけは BLAS に任せる。
-- ほかの形は、これまでどおり Np.dot
dot :: NDArray -> NDArray -> NDArray
dot a b = case (Np.shape a, Np.shape b) of
  ([_, k], [k', _]) | k == k' -> fromMatrix (toMatrix a LA.<> toMatrix b)
  _                           -> Np.dot a b
-- ANCHOR_END: dot
-- ANCHOR_END: all
