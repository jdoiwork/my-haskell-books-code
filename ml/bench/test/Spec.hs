-- ANCHOR: head
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck

import qualified Data.Massiv.Array as A

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
import qualified MiniNumpy.Blas as Blas
import Bench.Kernels
import BenchAnswers
import MiniTorch.Digits
import MiniTorch.Mnist
import MiniTorch.NN
import MiniTorch.Tensor

near :: Double -> Double -> Double -> Bool
near tolerance a b = abs (a - b) < tolerance

-- 🎲 A random matrix of small numbers.
-- 小さな数の、でたらめな行列
genMatrix :: Int -> Int -> Gen NDArray
genMatrix r c = do
  xs <- vectorOf (r * c) (choose (-2, 2))
  pure (Np.reshape [r, c] (Np.array xs))

-- 🐢 The loss with one weight nudged by h (for the numeric slope).
-- 重みを1つだけ h ずらしたときの損失（数値微分のため）
lossWith :: Params -> NDArray -> [Int] -> String -> Int -> Double -> Double
lossWith ps x ys name k h =
  let w   = ps Map.! name
      ws  = [ if j == k then v + h else v
            | (j, v) <- zip [0 ..] (Np.toList w) ]
      ps' = Map.insert name (Np.reshape (Np.shape w) (Np.array ws)) ps
  in Np.item (tvalue (crossEntropy (mlp ps' x) ys))
-- ANCHOR_END: head

main :: IO ()
main = hspec $ do
  -- 「速い道のブロードキャストも、NumPy の決まりどおり」
  describe "broadcasting fast paths" $ do
    it "adds a row to every row" $
      property $ forAll (genMatrix 3 4) $ \x -> forAll (genMatrix 1 4) $ \v ->
        let row   = Np.reshape [4] v
            tiled = Np.array2 (replicate 3 (Np.toList row))
        in Np.toList (x + row) === Np.toList (x + tiled)
    it "divides every row by its own number" $
      property $ forAll (genMatrix 3 4) $ \x -> forAll (genMatrix 3 1) $ \v ->
        let tiled = Np.array2 [ replicate 4 c | c <- Np.toList v ]
        in Np.toList (x / v) === Np.toList (x / tiled)

  -- ANCHOR: gradcheck
  -- 「テンソルの傾きは、小さくずらして測った傾きと同じ」
  describe "tensor gradients" $
    it "agree with the numeric slope" $
      property $ forAll (chooseInt (0, 35 * 8 - 1)) $ \k ->
        let (x, ys) = makeDigits 3 0.1 20
            ps      = initParams 5 [("fc1", 35, 8), ("fc2", 8, 10)]
            grads   = backward (crossEntropy (mlp ps x) ys)
            auto    = Np.toList (grads Map.! "fc1.weight") !! k
            h       = 1e-6
            numeric = (lossWith ps x ys "fc1.weight" k h
                       - lossWith ps x ys "fc1.weight" k (-h)) / (2 * h)
        in counterexample (show (auto, numeric)) (near 1e-6 auto numeric)
  -- ANCHOR_END: gradcheck

  -- 「学ぶと、損失が減り、数字を見分けられる」
  describe "training" $
    it "lowers the loss and learns the digits" $ do
      let (x, ys) = makeDigits 1 0.1 200
          start   = initParams 7 [("fc1", 35, 32), ("fc2", 32, 10)]
          lossOf ps = Np.item (tvalue (crossEntropy (mlp ps x) ys))
          step ps = sgdStep 0.5 ps (backward (crossEntropy (mlp ps x) ys))
          final   = iterate step start !! 100
      lossOf final `shouldSatisfy` (< lossOf start / 10)
      accuracy final x ys `shouldSatisfy` (> 0.95)

  -- ANCHOR: race
  -- 「比べっこの選手は、どれも同じ答えを出す」（形もいろいろ）
  describe "the racers agree" $ do
    let size1 = chooseInt (1, 6)
        sizes = (,,) <$> size1 <*> size1 <*> size1
        same a b = Np.shape a == Np.shape b
                   && and (zipWith (near 1e-9) (Np.toList a) (Np.toList b))
    it "BLAS and the three loops multiply like Np.dot" $
      property $ forAll sizes $ \(r, k, c) ->
        forAll (genMatrix r k) $ \a -> forAll (genMatrix k c) $ \b ->
          same (Blas.dot a b) (Np.dot a b)
            && same (naiveMatmul a b) (Np.dot a b)
    it "Blas.dot gives other shapes to Np.dot" $
      property $ forAll (genMatrix 3 4) $ \a -> forAll (genMatrix 4 1) $ \v ->
        let vec = Np.reshape [4] v
        in same (Blas.dot a vec) (Np.dot a vec)
    it "every softmax gives the same rows, and each row adds up to 1" $
      property $ forAll ((,) <$> size1 <*> size1) $ \(r, c) ->
        forAll (genMatrix r c) $ \x ->
          let want = softmaxRowsNp x
          in all (\got -> same got want)
                 [ softmaxRowsV x
                 , softmaxRowsMassiv A.Seq x
                 , softmaxRowsMassiv A.Par x ]
             && all (near 1e-9 1) (Np.toList (Np.sumAxis 1 want))
  -- ANCHOR_END: race

  -- 「トレーニングの解答」
  describe "answers" $ do
    let pairs = (,) <$> chooseInt (1, 6) <*> chooseInt (1, 6)
        same a b = Np.shape a == Np.shape b
                   && and (zipWith (near 1e-9) (Np.toList a) (Np.toList b))
    it "dotMV multiplies a matrix and a vector like Np.dot" $
      property $ forAll pairs $ \(r, k) ->
        forAll (genMatrix r k) $ \a -> forAll (genMatrix k 1) $ \v ->
          let vec = Np.reshape [k] v
          in same (dotMV a vec) (Np.dot a vec)
    it "softmaxRowsST gives the same rows as MiniNumpy" $
      property $ forAll pairs $ \(r, c) ->
        forAll (genMatrix r c) $ \x ->
          same (softmaxRowsST x) (softmaxRowsNp x)

  -- 「トレーニング」
  describe "MNIST files" $ do
    -- 手で作った 2 枚・1×2 点の画像ファイルを読む
    it "reads a tiny image file" $ do
      let header = [0, 0, 8, 3, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 2]
          bytes  = BS.pack (header <> [0, 255, 51, 0])
      fmap (\a -> (Np.shape a, Np.toList a)) (parseImages bytes)
        `shouldBe` Right ([2, 2], [0, 1, 0.2, 0])
    -- ラベルのファイルを読む
    it "reads a tiny label file" $
      parseLabels (BS.pack [0, 0, 8, 1, 0, 0, 0, 2, 3, 7])
        `shouldBe` Right [3, 7]
    -- 途中で切れたファイルは断る
    it "rejects a file that is cut short" $
      parseLabels (BS.pack [0, 0, 8, 1, 0, 0, 0, 5, 3, 7])
        `shouldBe` Left "the file is cut short"
    -- rowsFrom は、その行だけを切り出す
    it "rowsFrom cuts out rows" $
      Np.toList (Np.rowsFrom 1 2 (Np.reshape [4, 2] (Np.arange 8)))
        `shouldBe` [2, 3, 4, 5]
  -- ANCHOR_END: mnist
