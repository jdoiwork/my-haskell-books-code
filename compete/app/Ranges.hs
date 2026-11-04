-- ANCHOR: all
-- 📏 Problem 1: sums of damage in ranges.
--    This file is complete on its own, so you can submit it as it is.
-- 問題1：区間のダメージの合計。
-- このファイルだけで完結しているので、そのまま提出できる
module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.Maybe            (mapMaybe)
import Data.Vector.Unboxed   qualified as VU

-- ANCHOR: read
-- 🔢 All the numbers in some text, read fast with ByteString.
-- テキストの中の数を全部、ByteString で速く読む
ints :: BS.ByteString -> [Int]
ints = map fst . mapMaybe BS.readInt . BS.words
-- ANCHOR_END: read

-- ANCHOR: main
main :: IO ()
main = do
  input <- BS.getContents
  case ints input of
    (n : q : rest) -> do
      let (damages, queries) = splitAt n rest
          sums = VU.fromList (scanl (+) 0 damages)
          answer (l, r) = sums VU.! r - sums VU.! (l - 1)
      BS.putStr (BS.unlines (map (BS.pack . show . answer)
                                 (take q (pairs queries))))
    _ -> pure ()
  where
    pairs (a : b : rest) = (a, b) : pairs rest
    pairs _              = []
-- ANCHOR_END: main
-- ANCHOR_END: all
