-- A first try of hasktorch: multiply two matrices with libtorch.
-- hasktorch を試す：libtorch で行列を掛ける
module Main (main) where

import Torch

main :: IO ()
main = do
  let a = asTensor ([[1, 2], [3, 4]] :: [[Double]])
      b = asTensor ([[5, 6], [7, 8]] :: [[Double]])
  print (matmul a b)
