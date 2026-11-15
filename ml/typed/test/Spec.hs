-- ANCHOR: head
module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat)
import Test.Hspec
import Test.QuickCheck

import Answers
import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
import MiniTorch.Digits
import MiniTorch.NN
import MiniTorch.Tensor
import qualified MiniTorch.Typed as T
import qualified MiniTorch.TypedNN as TN
import qualified TypedAnswers as TA

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

-- 📐 The array inside has the shape the type says.
-- 中の配列が、型に書いた形をしている
shapeOk :: forall r c. (KnownNat r, KnownNat c) => T.Matrix r c -> Bool
shapeOk m =
  Np.shape (T.toNDArray m) == [T.natInt (Proxy @r), T.natInt (Proxy @c)]

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
                                 | (j, e) <- zip [0 :: Int ..] ws ])
          ws       = Np.toList w
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

  -- ANCHOR: typed
  -- 「型で次元を守る」
  describe "typed matrices" $ do
    -- 形がちがう配列は、門番が止める（行だけ、列だけ、次元の数）
    it "the gate rejects an array of the wrong shape" $ do
      let gate a = fmap T.toNDArray (T.fromNDArray @2 @3 a)
      gate (Np.zeros [3, 3]) `shouldBe` Left "expected shape [2,3], got [3,3]"
      gate (Np.zeros [2, 2]) `shouldBe` Left "expected shape [2,3], got [2,2]"
      gate (Np.zeros [6]) `shouldBe` Left "expected shape [2,3], got [6]"
      fmap T.vectorToNDArray (T.vectorFromNDArray @3 (Np.zeros [4]))
        `shouldBe` Left "expected shape [3], got [4]"
    -- 型の大きな数は、小さな数に読まれない。長さのそろわない行も Left
    it "the gate reads huge numbers in types and ragged rows" $ do
      let huge = T.matrix @18446744073709551618 @3 [[1, 2, 3], [4, 5, 6]]
      fmap T.toNDArray huge
        `shouldSatisfy` either (const True) (const False)
      fmap T.toNDArray (T.matrix @2 @2 [[1], [2, 3]])
        `shouldBe` Left "the rows have different lengths"
    -- 行の数は、データから型になる。列の数は、厳しく確かめる
    it "withRows turns the number of rows into a type" $ do
      let (x, _) = makeDigits 1 0.1 7
      T.withRows @35 x (\m -> Np.shape (T.toNDArray m))
        `shouldBe` Right [7, 35]
      T.withRows @35 (Np.zeros [4, 36]) (const ())
        `shouldBe` Left "expected shape [4,35], got [4,36]"
    -- 型のあるネットワークは、型のない mlp と同じ数を出す
    it "the typed network gives the same numbers as the untyped one" $ do
      let (x, ys) = makeDigits 1 0.1 20
          start   = TN.initMLP @35 @32 @10 7
          -- Compare after one step: the biases start at 0, so a
          -- forgotten bias would not show at the start.
          -- 1歩学んでから比べる。バイアスは 0 から始まるので、
          -- 最初のままでは、バイアスの足し忘れが見えない
          both m =
            let net = snd (TN.trainStep 0.5 start m ys)
            in ( T.toNDArray (TN.mlp net m)
               , tvalue (mlp (TN.toParams net) x) )
      fmap (uncurry (==)) (T.withRows @35 x both)
        `shouldBe` Right True
    -- 型のある1歩は、型のない1歩と同じ
    it "one typed step is the same as one untyped step" $ do
      let (x, ys) = makeDigits 1 0.1 50
          net     = TN.initMLP @35 @32 @10 7
          ps0     = TN.toParams net
          stepped = sgdStep 0.5 ps0 (backward (crossEntropy (mlp ps0 x) ys))
      T.withRows @35 x (\m -> TN.toParams (snd (TN.trainStep 0.5 net m ys)))
        `shouldBe` Right stepped
    -- vstack のあと splitRows すると、元の2つに戻る
    it "splitRows undoes vstack" $ do
      let (x, _) = makeDigits 1 0.1 2
          (y, _) = makeDigits 2 0.1 3
          parts  = do
            a <- T.fromNDArray @2 @35 x
            b <- T.fromNDArray @3 @35 y
            let (a', b') = TA.splitRows @2 @3 (T.vstack a b)
            pure (T.toNDArray a', T.toNDArray b')
      parts `shouldBe` Right (x, y)
    -- 3層のネットワークも、行ごとに 10 個の数を出す
    it "the typed mlp3 gives 10 numbers per row" $ do
      let (x, _) = makeDigits 1 0.1 5
          first  = TN.fc1 (TN.initMLP @35 @16 @16 1)
          middle = TN.fc2 (TN.initMLP @35 @16 @16 2)
          final  = TN.fc2 (TN.initMLP @16 @16 @10 3)
          net    = TA.MLP3 first middle final
      T.withRows @35 x (\m -> Np.shape (T.toNDArray (TA.mlp3 net m)))
        `shouldBe` Right [5, 10]
    -- 形を変える計算は、どれも型に書いた形の配列を返す
    it "every operation returns the shape its type says" $ do
      let parts = do
            a <- T.matrix @2 @3 [[1, 2, 3], [4, 5, 6]]
            b <- T.matrix @3 @4 (replicate 3 [1, 0, -1, 2])
            c <- T.matrix @1 @3 [[7, 8, 9]]
            v <- T.vectorFromNDArray @3 (Np.array [10, 20, 30])
            pure [ shapeOk (T.matmul a b), shapeOk (T.transposeM a)
                 , shapeOk (T.vstack a c), shapeOk (T.addRow a v)
                 , shapeOk (T.mapMatrix negate a), shapeOk (T.add a a) ]
      parts `shouldBe` Right (replicate 6 True)
    -- add は足し算、addRow はどの行にも同じ行を足す
    it "add and addRow add numbers" $ do
      let sums = do
            a <- T.matrix @2 @2 [[1, 2], [3, 4]]
            v <- T.vectorFromNDArray @2 (Np.array [10, 20])
            pure ( Np.toList (T.toNDArray (T.add a a))
                 , Np.toList (T.toNDArray (T.addRow a v)) )
      sums `shouldBe` Right ([2, 4, 6, 8], [11, 22, 13, 24])
    -- 型のある predict は、型のない mlp の、行ごとのいちばん大きな数
    it "predict picks the biggest number of each row" $ do
      let (x, _) = makeDigits 2 0.1 30
          net    = TN.initMLP @35 @32 @10 7
      T.withRows @35 x (TN.predict net)
        `shouldBe` Right (Np.argmaxAxis1 (tvalue (mlp (TN.toParams net) x)))
  -- ANCHOR_END: typed
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
