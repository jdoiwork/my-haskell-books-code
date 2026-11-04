module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck

import Answers
import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
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

  -- 「トレーニング」
  describe "answers" $ do
    it "batches cut 1000 rows into 10 batches of 100" $ do
      let (x, ys) = makeDigits 1 0.1 1000
      map (Np.nrows . fst) (batches 100 x ys) `shouldBe` replicate 10 100
    it "mlp3 gives 10 numbers per row" $ do
      let (x, _) = makeDigits 1 0.1 5
          layers = [("fc1", 35, 16), ("fc2", 16, 16), ("fc3", 16, 10)]
          ps     = initParams 1 layers
      Np.shape (tvalue (mlp3 ps x)) `shouldBe` [5, 10]
    it "the confusion table adds up to the number of digits" $ do
      let (x, ys) = makeDigits 2 0.1 50
          ps = initParams 7 [("fc1", 35, 32), ("fc2", 32, 10)]
      sum (map sum (confusion ps x ys)) `shouldBe` 50

  -- ANCHOR: mnist
  -- 「本物の MNIST に挑む」
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
