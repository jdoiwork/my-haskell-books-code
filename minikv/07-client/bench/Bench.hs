-- ANCHOR: all
-- | How much does pipelining help? 10000 INCRs, one by one and in
--   batches of 100.   cabal run minikv-bench
-- パイプラインは、どれだけ効く？ INCR を 1 万回、1つずつと、100 個ずつで
module Main (main) where

import Control.Monad (replicateM_)
import GHC.Clock (getMonotonicTime)
import Text.Printf (printf)

import MiniKv.Client
import MiniKv.Server (withServer)

main :: IO ()
main = withServer $ \port -> withConnection port $ \conn -> do
  t0 <- getMonotonicTime
  replicateM_ 10000 (request conn ["INCR", "one-by-one"])
  t1 <- getMonotonicTime
  replicateM_ 100 (requestMany conn (replicate 100 ["INCR", "pipelined"]))
  t2 <- getMonotonicTime
  printf "one by one: %.2f s\n" (t1 - t0)
  printf "pipelined:  %.2f s\n" (t2 - t1)
  answers <- requestMany conn [["GET", "one-by-one"], ["GET", "pipelined"]]
  print answers
-- ANCHOR_END: all
