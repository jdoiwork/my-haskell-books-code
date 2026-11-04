-- ANCHOR: all
-- replicateM: do the same IO action n times.
-- replicateM：同じ IO を n 回くり返す
import Control.Monad (replicateM)

-- printf: print numbers in a fixed format, like C and Python.
-- printf：C や Python のように、決まった書式で数を表示する
import Text.Printf (printf)

-- 📊 Problem 3: read N lines of "name,score", and print the average
--    score with exactly two decimal places.
-- 問題3："名前,点数" の行を N 行読み、平均点を小数点以下2桁で表示する
main :: IO ()
main = do
  n <- readLn :: IO Int
  rows <- replicateM n getLine
  let scores  = map scoreOf rows
      total   = sum scores
      average = fromIntegral total / fromIntegral n :: Double
  printf "%.2f\n" average

-- 🔢 The score part of "Lambda,85": everything after the comma.
-- "Lambda,85" の点数の部分：カンマより後ろ全部
scoreOf :: String -> Int
scoreOf line = read (drop 1 (dropWhile (/= ',') line))
-- ANCHOR_END: all
