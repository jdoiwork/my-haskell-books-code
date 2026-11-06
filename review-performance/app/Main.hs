-- ANCHOR: all
-- 📒 Read the ledger from standard input, and print the report.
-- 標準入力から決算を読み、報告を出す
module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict       qualified as Map

import Ledger

main :: IO ()
main = do
  input <- BS.getContents
  let records = parseRecords input
      sums    = totalsParallel 8 records
  -- 「記録：何件、勇者：何人」
  putStrLn ("records: " ++ show (length records)
            ++ ", heroes: " ++ show (Map.size sums))
  mapM_ (\(h, g) -> BS.putStrLn (h <> BS.pack (": " ++ show g)))
        (topThree sums)
-- ANCHOR_END: all
