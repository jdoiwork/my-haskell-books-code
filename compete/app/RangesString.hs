-- ANCHOR: all
-- 🐢 Problem 1 again, reading the input as a String (for comparison).
-- 問題1をもう一度。入力を String で読む（比べるため）
module Main (main) where

import Data.Vector.Unboxed qualified as VU

main :: IO ()
main = do
  input <- getContents
  case map read (words input) of
    (n : q : rest) -> do
      let (damages, queries) = splitAt n rest
          sums = VU.fromList (scanl (+) 0 damages)
          answer (l, r) = sums VU.! r - sums VU.! (l - 1)
      putStr (unlines (map (show . answer) (take q (pairs queries))))
    _ -> pure ()
  where
    pairs (a : b : rest) = (a, b) : pairs rest
    pairs _              = []
-- ANCHOR_END: all
