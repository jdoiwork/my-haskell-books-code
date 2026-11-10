-- ANCHOR: all
-- 📒 Read the ledger from standard input, and print the report.
--    ledger       : add up in parallel (8 parts)
--    ledger once  : count and add up in one walk
-- 標準入力から決算を読み、報告を出す。
-- ledger は並列に（8つに分けて）、ledger once は1回たどるだけで足す
module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict       qualified as Map
import System.Environment    (getArgs)

import Ledger

main :: IO ()
main = do
  args  <- getArgs
  input <- BS.getContents
  let records       = parseRecords input
      (count, sums) = case args of
        ["once"] -> countAndTotals records
        _        -> (length records, totalsParallel 8 records)
  -- 「記録：何件、勇者：何人」
  putStrLn ("records: " ++ show count
            ++ ", heroes: " ++ show (Map.size sums))
  mapM_ (\(h, g) -> BS.putStrLn (h <> BS.pack (": " ++ show g)))
        (topThree sums)
-- ANCHOR_END: all
