module Main (main) where

-- ANCHOR: imports
import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Answers
import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
-- ANCHOR_END: imports

-- 🎲 A random 2-D array with small whole numbers.
-- 小さな整数の、でたらめな2次元の配列
genMatrix :: Int -> Int -> Gen NDArray
genMatrix r c = do
  xs <- vectorOf (r * c) (fromIntegral <$> chooseInt (-5, 5))
  pure (Np.reshape [r, c] (Np.array xs))

genShape :: Gen (Int, Int)
genShape = (,) <$> chooseInt (1, 4) <*> chooseInt (1, 4)

-- 🐢 Matrix product with lists of lists (the reference).
-- リストのリストで計算する、行列の積（お手本）
naiveDot :: [[Double]] -> [[Double]] -> [[Double]]
naiveDot a b = [ [ sum (zipWith (*) r c) | c <- cols ] | r <- a ]
  where cols = foldr (zipWith (:)) (repeat []) b

rowsOf :: NDArray -> [[Double]]
rowsOf a = [ Np.toList (Np.row i a) | i <- [0 .. Np.nrows a - 1] ]

main :: IO ()
main = hspec $ do
  -- 「NumPy と同じ答え」
  describe "MiniNumpy" $ do
    -- ANCHOR: examples
    let a = Np.array2 [[1, 2, 3], [4, 5, 6]]
    -- 「ブロードキャストして足す」
    it "broadcasts like NumPy" $
      map show [ a + Np.array [10, 20, 30], a * 2
               , a + Np.array2 [[100], [200]] ]
        `shouldBe`
          [ "array([[11., 22., 33.], [14., 25., 36.]])"
          , "array([[2., 4., 6.], [8., 10., 12.]])"
          , "array([[101., 102., 103.], [204., 205., 206.]])" ]

    -- 「行列の積と、軸ごとの合計」
    it "multiplies matrices and sums along an axis" $
      map show [Np.dot a (Np.transpose a), Np.sumAxis 0 a, Np.sumAxis 1 a]
        `shouldBe`
          [ "array([[14., 32.], [32., 77.]])"
          , "array([5., 7., 9.])", "array([6., 15.])" ]

    -- 「形が合わなければ、止まる」
    it "stops when the shapes do not fit" $ do
      evaluate (a + Np.array [1, 2]) `shouldThrow` anyErrorCall
      evaluate (Np.reshape [4, 2] a) `shouldThrow` anyErrorCall
    -- ANCHOR_END: examples

    -- ANCHOR: props
    -- 「行列の積は、お手本と同じ」
    it "dot agrees with the list-of-lists reference" $
      property $ forAll genShape $ \(r, k) ->
        forAll (chooseInt (1, 4)) $ \c ->
        forAll (genMatrix r k) $ \x -> forAll (genMatrix k c) $ \y ->
          rowsOf (Np.dot x y) === naiveDot (rowsOf x) (rowsOf y)

    -- 「行をブロードキャストして足すのは、行を並べて足すのと同じ」
    it "adding a row is adding that row to every row" $
      property $ forAll genShape $ \(r, c) ->
        forAll (genMatrix r c) $ \x -> forAll (genMatrix 1 c) $ \v ->
          let vRow  = Np.reshape [c] v
              tiled = Np.array2 (replicate r (Np.toList vRow))
          in x + vRow === x + tiled

    -- 「2回入れかえると、元に戻る」
    it "transposing twice gives the same array" $
      property $ forAll genShape $ \(r, c) -> forAll (genMatrix r c) $ \x ->
        Np.transpose (Np.transpose x) === x
    -- ANCHOR_END: props

  -- 「トレーニング」
  describe "answers" $ do
    it "eye times a matrix is the matrix" $
      property $ forAll genShape $ \(r, c) -> forAll (genMatrix r c) $ \x ->
        Np.dot (eye r) x === x
    it "clip cuts the ends" $
      show (clip 0 1 (Np.array [-1, 0.5, 2]))
        `shouldBe` "array([0., 0.5, 1.])"
    it "softmax rows add up to 1" $
      property $ forAll genShape $ \(r, c) -> forAll (genMatrix r c) $ \x ->
        let sums = Np.sumAxis 1 (softmaxRows x)
        in all (\t -> abs (t - 1) < 1e-9) (Np.toList sums)
