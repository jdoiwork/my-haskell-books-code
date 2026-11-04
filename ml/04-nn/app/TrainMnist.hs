-- ANCHOR: all
-- | Train on the real MNIST, 100 images at a time.
--   cabal run train-mnist -- ../data/mnist
-- 本物の MNIST で、100 枚ずつ学ばせる
module Main (main) where

import Control.Monad (foldM, when)
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Text.Printf (printf)

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
import MiniTorch.Mnist
import MiniTorch.NN
import MiniTorch.Tensor

-- ANCHOR: load
-- 📂 Read the four files, or stop with the reason.
-- 4つのファイルを読む。読めなければ、理由を出して止まる
loadOrDie :: FilePath -> String -> IO (NDArray, [Int])
loadOrDie dir prefix = do
  result <- loadMnist (dir </> prefix <> "-images-idx3-ubyte")
                      (dir </> prefix <> "-labels-idx1-ubyte")
  case result of
    Right pair -> pure pair
    Left err   -> do
      putStrLn ("cannot read " <> prefix <> ": " <> err)
      exitFailure
-- ANCHOR_END: load

-- ANCHOR: epoch
-- 🔁 One pass over the data, 100 rows at a time, in order.
-- データを先頭から 100 行ずつ、1周する
epoch :: NDArray -> [Int] -> Params -> IO Params
epoch x labels start = foldM step start (zip [0 ..] (chunks 100 labels))
  where
    step :: Params -> (Int, [Int]) -> IO Params
    step ps (k, ys) = do
      let batch = Np.rowsFrom (k * 100) (length ys) x
          loss  = crossEntropy (mlp ps batch) ys
      when (k `mod` 100 == 0) $
        printf "batch %3d  loss %.4f\n" k (Np.item (tvalue loss))
      pure (sgdStep 0.1 ps (backward loss))

-- ✂️ [1..250] in chunks of 100: [1..100], [101..200], [201..250].
-- 100 個ずつに切る
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (front, rest) = splitAt n xs in front : chunks n rest
-- ANCHOR_END: epoch

-- ANCHOR: main
main :: IO ()
main = do
  args <- getArgs
  let dir = case args of
              (d : _) -> d
              []      -> "../data/mnist"
  (xTrain, yTrain) <- loadOrDie dir "train"
  (xTest, yTest)   <- loadOrDie dir "t10k"
  printf "train %s, test %s\n"
    (show (Np.shape xTrain)) (show (Np.shape xTest))
  let start = initParams 7 [("fc1", 784, 64), ("fc2", 64, 10)]
  t0 <- getMonotonicTime
  final <- epoch xTrain yTrain start
  t1 <- getMonotonicTime
  printf "test accuracy %.4f (one epoch in %.1f s)\n"
    (accuracy final xTest yTest) (t1 - t0)
-- ANCHOR_END: main
-- ANCHOR_END: all
