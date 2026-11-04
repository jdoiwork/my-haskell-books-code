-- ANCHOR: all
-- 🔢 Count primes up to 3,000,000, in one part and in parallel.
-- 300万までの素数を、1つのまとまりで、そして並列で数える
module Main (main) where

import GHC.Clock          (getMonotonicTime)
import System.Environment (getArgs)
import Text.Printf        (printf)

import Primes

main :: IO ()
main = do
  args  <- getArgs
  start <- getMonotonicTime
  let n = 3000000
      answer = case args of
        ["parallel"] -> countPrimesParallel n
        _            -> countPrimes n
  print answer
  end <- getMonotonicTime
  printf "%.2f s\n" (end - start)
-- ANCHOR_END: all
