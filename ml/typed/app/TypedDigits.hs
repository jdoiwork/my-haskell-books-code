-- ANCHOR: head
-- ANCHOR: all
-- | Train the same network as the last chapter, but with the layer
--   sizes in the type.   cabal run typed-digits
-- 前の章と同じネットワークを、層の大きさを型に書いて学ばせる
module Main (main) where

import Control.Monad (when)
import Text.Printf (printf)

import MiniTorch.Digits
import MiniTorch.Typed
import MiniTorch.TypedNN
-- ANCHOR_END: head

-- ANCHOR: model
-- 🧠 35 inputs, 32 hidden, 10 outputs, written once, in the type.
-- 入力 35、真ん中 32、出力 10。型に1回だけ書く
type DigitNet = MLP 35 32 10
-- ANCHOR_END: model

-- ANCHOR: train
-- 🔁 300 steps on all n rows. n can be any number; the type of the
--    network never changes.
-- n 行すべてで 300 歩。n はいくつでもよく、ネットワークの型は変わらない
train :: Matrix n 35 -> [Int] -> IO DigitNet
train x labels = go (initMLP 7) 0
  where
    go :: DigitNet -> Int -> IO DigitNet
    go net epoch
      | epoch > 300 = pure net
      | otherwise = do
          let (loss, net') = trainStep 0.5 net x labels
          when (epoch `mod` 100 == 0) $
            printf "epoch %3d  loss %.4f\n" epoch loss
          go net' (epoch + 1)
-- ANCHOR_END: train

-- ANCHOR: accuracy
-- 🏅 The share of rows the network gets right.
-- 当てた行の割合
accuracy :: DigitNet -> Matrix n 35 -> [Int] -> Double
accuracy net x labels =
  let right = length (filter id (zipWith (==) (predict net x) labels))
  in  fromIntegral right / fromIntegral (length labels)
-- ANCHOR_END: accuracy

-- ANCHOR: main
main :: IO ()
main = do
  let (xTrain, yTrain) = makeDigits 1 0.1 1000
      (xTest, yTest)   = makeDigits 2 0.1 200
  -- The number of rows comes from the data, so it goes through withRows.
  -- 行の数はデータから来るので、withRows を通す
  case withRows @35 xTrain (\x -> train x yTrain) of
    Left err     -> putStrLn ("bad training data: " <> err)
    Right action -> do
      net <- action
      case withRows @35 xTest (\x -> accuracy net x yTest) of
        Left err  -> putStrLn ("bad test data: " <> err)
        Right acc -> printf "test accuracy %.3f\n" acc
      -- The gate stops data with the wrong number of columns.
      -- 列の数がちがうデータは、門番が止める
      let (wide, _) = makeDigits 3 0.1 4
      case withRows @36 wide (const ()) of
        Left err -> putStrLn ("rejected: " <> err)
        Right () -> putStrLn "accepted (this should not happen)"
-- ANCHOR_END: main
-- ANCHOR_END: all
