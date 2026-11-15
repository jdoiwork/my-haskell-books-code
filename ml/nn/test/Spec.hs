-- ANCHOR: head
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
-- 🐢 The numeric slope of the loss for one number k of one parameter.
-- パラメーター1つの、k 番目の数についての、損失の数値微分
numericSlope :: Params -> NDArray -> [Int] -> String -> Int -> Double
numericSlope ps x ys name k =
  let h = 1e-6
  in (lossWith ps x ys name k h - lossWith ps x ys name k (-h)) / (2 * h)
-- ANCHOR_END: head

-- ANCHOR: main
main :: IO ()
main = hspec $ do
-- ANCHOR_END: main
  -- ANCHOR: fastpaths
  -- 「速い道のブロードキャストも、NumPy の決まりどおり」
  describe "broadcasting fast paths" $ do
    it "subtracts a row from every row" $
      property $ forAll (genMatrix 3 4) $ \x -> forAll (genMatrix 1 4) $ \v ->
        let row   = Np.reshape [4] v
            tiled = Np.array2 (replicate 3 (Np.toList row))
        in Np.toList (x - row) === Np.toList (x - tiled)
    it "divides every row by its own number" $
      property $ forAll (genMatrix 3 4) $ \x -> forAll (genMatrix 3 1) $ \v ->
        let tiled = Np.array2 [ replicate 4 c | c <- Np.toList v ]
        in Np.toList (x / v) === Np.toList (x / tiled)
  -- ANCHOR_END: fastpaths

  -- ANCHOR: gradcheck
  -- 「テンソルの傾きは、小さくずらして測った傾きと同じ」
  describe "tensor gradients" $ do
    it "agree with the numeric slope, for every parameter" $
      property $ forAll (elements names) $ \name ->
        forAll (chooseInt (0, Np.size (ps Map.! name) - 1)) $ \k ->
        let auto = Np.toList (grads Map.! name) !! k
            num  = numericSlope ps gx gys name k
        in counterexample (show (name, auto, num)) (near 1e-6 auto num)

    it "adds up the gradients of a weight used twice" $ do
      let w       = Np.reshape [2, 2] (Np.array [0.5, -0.3, 0.2, 0.8])
          x1      = constant (Np.array2 [[1, 2]])
          twice v = crossEntropy (matmul (matmul x1 v) v) [0]
          bumped d = Np.reshape [2, 2]
                       (Np.array [ if j == 0 then e + d else e
                                 | (j, e) <- zip [0 :: Int ..] (Np.toList w) ])
          lossAt d = Np.item (tvalue (twice (constant (bumped d))))
          gw      = backward (twice (param "w" w)) Map.! "w"
          auto    = take 1 (Np.toList gw)
          num     = (lossAt 1e-6 - lossAt (-1e-6)) / 2e-6
      map (near 1e-6 num) auto `shouldBe` [True]

    it "keeps the loss finite for far-apart scores" $ do
      let lossOf ys = Np.item (tvalue (crossEntropy
                        (constant (Np.array2 [[1000, 0]])) ys))
      map lossOf [[0], [1]] `shouldBe` [0, 1000]
  -- ANCHOR_END: gradcheck

  -- ANCHOR: training
  -- 「学ぶと、損失が減り、数字を見分けられる」
  describe "training" $
    it "lowers the loss and learns the digits" $ do
      let (x2, ys2) = makeDigits 1 0.1 200
          start   = initParams 7 [("fc1", 35, 32), ("fc2", 32, 10)]
          lossOf q = Np.item (tvalue (crossEntropy (mlp q x2) ys2))
          step q  = sgdStep 0.5 q (backward (crossEntropy (mlp q x2) ys2))
          final   = iterate step start !! 100
      lossOf final `shouldSatisfy` (< lossOf start / 10)
      accuracy final x2 ys2 `shouldSatisfy` (> 0.95)
  -- ANCHOR_END: training

  -- ANCHOR: answers
  -- 「トレーニング」
  describe "answers" $ do
    -- 「1000 行を、100 行ずつ 10 束に」
    it "batches cut 1000 rows into 10 batches of 100" $ do
      let (x2, ys2) = makeDigits 1 0.1 1000
      map (Np.nrows . fst) (batches 100 x2 ys2) `shouldBe` replicate 10 100
    -- 「100 行ずつ 30 周で、テストの正解率 0.985」
    it "learns as well with mini-batches (30 epochs)" $ do
      let (x2, ys2) = makeDigits 1 0.1 1000
          (tx, tys) = makeDigits 2 0.1 200
          start = initParams 7 [("fc1", 35, 32), ("fc2", 32, 10)]
          bs    = batches 100 x2 ys2
          final = iterate (\q -> trainEpoch 0.5 q bs) start !! 30
      accuracy final tx tys `shouldBe` 0.985
    -- 「mlp3 は、行ごとに 10 個の数を返し、正解率も測れる」
    it "mlp3 gives 10 numbers per row" $ do
      let (x2, ys2) = makeDigits 1 0.1 5
          layers = [("fc1", 35, 16), ("fc2", 16, 16), ("fc3", 16, 10)]
          q      = initParams 1 layers
      Np.shape (tvalue (mlp3 q x2)) `shouldBe` [5, 10]
      accuracyWith mlp3 q x2 ys2 `shouldSatisfy` (\a -> a >= 0 && a <= 1)
    -- 「混同行列を足すと、数字の数」
    it "the confusion table adds up to the number of digits" $ do
      let (x2, ys2) = makeDigits 2 0.1 50
          q = initParams 7 [("fc1", 35, 32), ("fc2", 32, 10)]
      sum (map sum (confusion q x2 ys2)) `shouldBe` 50
  -- ANCHOR_END: answers

  -- ANCHOR: mnist
  -- 「本物の MNIST に挑む」
  describe "MNIST files" $ do
    -- 手で作った 2 枚・1×2 点の画像ファイルを読む
    it "reads a tiny image file" $ do
      let header = [0, 0, 8, 3, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 2]
          bytes  = BS.pack (header <> [0, 255, 51, 0])
      fmap (\a -> (Np.shape a, Np.toList a)) (parseImages bytes)
        `shouldBe` Right ([2, 2], [0, 1, 0.2, 0])
    -- 印がちがう、途中で切れた、大きさがあふれる画像のファイルは断る
    it "rejects a broken image file" $ do
      let header m = [0, 0, 8, m, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 2]
          huge     = [0, 0, 8, 3, 0, 0, 0, 4, 128, 0, 0, 0, 128, 0, 0, 0]
          shapeOf  = fmap Np.shape
      shapeOf (parseImages (BS.pack (header 1 <> [0, 255, 51, 0])))
        `shouldBe` Left "not an MNIST image file"
      shapeOf (parseImages (BS.pack (header 3 <> [0, 255, 51])))
        `shouldBe` Left "the file is cut short"
      shapeOf (parseImages (BS.pack huge))
        `shouldBe` Left "the file is cut short"
    -- ラベルのファイルを読む
    it "reads a tiny label file" $
      parseLabels (BS.pack [0, 0, 8, 1, 0, 0, 0, 2, 3, 7])
        `shouldBe` Right [3, 7]
    -- 途中で切れた、0〜9 でないラベルのファイルは断る
    it "rejects a broken label file" $ do
      parseLabels (BS.pack [0, 0, 8, 1, 0, 0, 0, 5, 3, 7])
        `shouldBe` Left "the file is cut short"
      parseLabels (BS.pack [0, 0, 8, 1, 0, 0, 0, 2, 3, 200])
        `shouldBe` Left "a label is not 0 .. 9"
    -- rowsFrom は、その行だけを切り出す
    it "rowsFrom cuts out rows" $
      Np.toList (Np.rowsFrom 1 2 (Np.reshape [4, 2] (Np.arange 8)))
        `shouldBe` [2, 3, 4, 5]
  -- ANCHOR_END: mnist
-- ANCHOR: gradcheckdata
  where
    -- The gradient check uses biases that are not 0, so that a bias
    -- that is not added (or its gradient) shows.
    -- 勾配チェックのバイアスは 0 でない数にする（足し忘れが見えるように）
    (gx, gys) = makeDigits 3 0.1 20
    biases  = ["fc1.bias", "fc2.bias"]
    ps      = Map.mapWithKey (\name p -> if name `elem` biases
                                         then p + 0.1 else p)
                (initParams 5 [("fc1", 35, 8), ("fc2", 8, 10)])
    names   = Map.keys ps
    grads   = backward (crossEntropy (mlp ps gx) gys)
-- ANCHOR_END: gradcheckdata
