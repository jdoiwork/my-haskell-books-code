-- ANCHOR: all
-- 🎒 Problem 3: the most value we can carry in the bag.
-- 問題3：袋に入れて運べる、いちばん大きな価値
module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.Maybe            (mapMaybe)

import Algo (bestBag)

ints :: BS.ByteString -> [Int]
ints = map fst . mapMaybe BS.readInt . BS.words

main :: IO ()
main = do
  input <- BS.getContents
  case ints input of
    (n : capacity : rest) -> print (bestBag capacity (take n (pairs rest)))
    _                     -> pure ()
  where
    pairs (a : b : rest) = (a, b) : pairs rest
    pairs _              = []
-- ANCHOR_END: all
