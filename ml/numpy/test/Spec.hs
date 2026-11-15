module Main (main) where

-- ANCHOR: imports
import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Answers
import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
-- ANCHOR_END: imports

-- ANCHOR: helpers
-- 🎲 A random 2-D array with small whole numbers.
-- 小さな整数の、でたらめな2次元の配列
genMatrix :: Int -> Int -> Gen NDArray
genMatrix r c = do
  xs <- vectorOf (r * c) (fromIntegral <$> chooseInt (-5, 5))
  pure (Np.reshape [r, c] (Np.array xs))

-- 📐 A random shape, 1 to 4 rows and columns.
-- でたらめな形（1〜4 行、1〜4 列）
genShape :: Gen (Int, Int)
genShape = (,) <$> chooseInt (1, 4) <*> chooseInt (1, 4)

-- 🐢 Matrix product with lists of lists (the reference).
-- リストのリストで計算する、行列の積（お手本）
naiveDot :: [[Double]] -> [[Double]] -> [[Double]]
naiveDot a b = [ [ sum (zipWith (*) r c) | c <- cols ] | r <- a ]
  where cols = foldr (zipWith (:)) (repeat []) b

-- 📋 A 2-D array as a list of rows.
-- 2次元の配列を、行のリストにする
rowsOf :: NDArray -> [[Double]]
rowsOf a = [ Np.toList (Np.row i a) | i <- [0 .. Np.nrows a - 1] ]
-- ANCHOR_END: helpers

-- ANCHOR: main
main :: IO ()
main = hspec $ do
  -- 「NumPy と同じ答え」
  describe "MiniNumpy" $ do
-- ANCHOR_END: main
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

    -- 「引き算と割り算は、順番どおり」
    it "keeps the order of - and /" $
      map show [2 - a, a - 2, 1 / Np.array [1, 2, 4]]
        `shouldBe`
          [ "array([[1., 0., -1.], [-2., -3., -4.]])"
          , "array([[-1., 0., 1.], [2., 3., 4.]])"
          , "array([1., 0.5, 0.25])" ]

    -- 「累乗と tanh も、数ごと」
    it "takes powers and tanh number by number" $
      map show [Np.array [-2, 3] ** 2, tanh (Np.array [0, 1000])]
        `shouldBe` ["array([4., 9.])", "array([0., 1.])"]

    -- 「行列の積と、軸ごとの合計」
    it "multiplies matrices and sums along an axis" $
      map show [Np.dot a (Np.transpose a), Np.sumAxis 0 a, Np.sumAxis 1 a]
        `shouldBe`
          [ "array([[14., 32.], [32., 77.]])"
          , "array([5., 7., 9.])", "array([6., 15.])" ]

    -- 「お客さんごとの売上（行列×ベクトル）と、平均」
    it "multiplies a matrix by a vector and takes the mean" $
      map show [ Np.dot (Np.array2 [[2, 1], [0, 3], [4, 2]])
                        (Np.array [10, 25])
               , Np.mean a ]
        `shouldBe` ["array([45., 75., 90.])", "3.5"]

    -- 「作って、形を変えて、のぞく」
    it "makes arrays and looks inside" $ do
      show (Np.reshape [2, 3] (Np.arange 6))
        `shouldBe` "array([[0., 1., 2.], [3., 4., 5.]])"
      show (Np.linspace 0 1 1) `shouldBe` "array([0.])"
      show (Np.zeros [3, 0]) `shouldBe` "array([[], [], []])"
      Np.at a [1, 2] `shouldBe` 6
      show (Np.row 1 a) `shouldBe` "array([4., 5., 6.])"

    -- 「形や添え字が合わなければ、止まる」
    it "stops when the shapes or the indices do not fit" $ do
      evaluate (a + Np.array [1, 2]) `shouldThrow` anyErrorCall
      evaluate (Np.reshape [4, 2] a) `shouldThrow` anyErrorCall
      evaluate (Np.at a [0, 3]) `shouldThrow` anyErrorCall
      evaluate (Np.zeros [-2, 3]) `shouldThrow` anyErrorCall
    -- ANCHOR_END: examples

    -- ANCHOR: props
    -- 「行列の積は、お手本と同じ」
    it "dot agrees with the list-of-lists reference" $
      property $ forAll genShape $ \(r, k) ->
        forAll (chooseInt (1, 4)) $ \c ->
        forAll (genMatrix r k) $ \x -> forAll (genMatrix k c) $ \y ->
          rowsOf (Np.dot x y) === naiveDot (rowsOf x) (rowsOf y)

    -- 「行列×ベクトルと、ベクトル・ベクトルも、お手本と同じ」
    it "dot with a vector agrees with the reference" $
      property $ forAll genShape $ \(r, k) ->
        forAll (genMatrix r k) $ \x -> forAll (genMatrix 1 k) $ \w ->
          let v  = Np.reshape [k] w
              vs = Np.toList v
          in Np.toList (Np.dot x v)
               === map (\row -> sum (zipWith (*) row vs)) (rowsOf x)
             .&&. Np.toList (Np.dot v v) === [sum (zipWith (*) vs vs)]

    -- 「行をブロードキャストして引くのは、行を並べて引くのと同じ」
    it "subtracting a row is subtracting it from every row" $
      property $ forAll genShape $ \(r, c) ->
        forAll (genMatrix r c) $ \x -> forAll (genMatrix 1 c) $ \v ->
          let vRow  = Np.reshape [c] v
              tiled = Np.array2 (replicate r (Np.toList vRow))
          in (x - vRow === x - tiled) .&&. (vRow - x === tiled - x)

    -- 「at は、行のリストの同じ場所と同じ」
    it "at agrees with the list of rows" $
      property $ forAll genShape $ \(r, c) -> forAll (genMatrix r c) $ \x ->
        forAll (chooseInt (0, r - 1)) $ \i ->
        forAll (chooseInt (0, c - 1)) $ \j ->
          Np.at x [i, j] === rowsOf x !! i !! j

    -- 「2回入れかえると、元に戻る」
    it "transposing twice gives the same array" $
      property $ forAll genShape $ \(r, c) -> forAll (genMatrix r c) $ \x ->
        Np.transpose (Np.transpose x) === x
    -- ANCHOR_END: props

  -- 「トレーニング」
  -- ANCHOR: answers
  describe "answers" $ do
    -- 「単位行列を掛けても、変わらない」
    it "eye times a matrix is the matrix" $
      property $ forAll genShape $ \(r, c) -> forAll (genMatrix r c) $ \x ->
        Np.dot (eye r) x === x
    -- 「はみ出しを切る（lo > hi なら全部 hi。np.clip と同じ）」
    it "clip cuts the ends" $ do
      show (clip 0 1 (Np.array [-1, 0.5, 2]))
        `shouldBe` "array([0., 0.5, 1.])"
      show (clip 2 1 (Np.array [0, 1, 3])) `shouldBe` "array([1., 1., 1.])"
    -- 「softmax の行は、足すと 1」
    it "softmax rows add up to 1" $
      property $ forAll genShape $ \(r, c) -> forAll (genMatrix r c) $ \x ->
        let sums = Np.sumAxis 1 (softmaxRows x)
        in all (\t -> abs (t - 1) < 1e-9) (Np.toList sums)
    -- 「softmax は、入力に合った確率になり、大きな数でもあふれない」
    it "softmax gives the right numbers, even for big inputs" $ do
      let close xs ys = and (zipWith (\p q -> abs (p - q) < 1e-9) xs ys)
      Np.toList (softmaxRows (Np.array2 [[0, log 3]]))
        `shouldSatisfy` close [0.25, 0.75]
      Np.toList (softmaxRows (Np.array2 [[1000, 1001]]))
        `shouldSatisfy` close (Np.toList (softmaxRows (Np.array2 [[0, 1]])))
  -- ANCHOR_END: answers
