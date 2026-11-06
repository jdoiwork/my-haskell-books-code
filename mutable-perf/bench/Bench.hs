-- ANCHOR: all
-- | Benchmarks: rewriting or not, safe or unsafe, which box?
-- ベンチマーク：書き換えるか、安全か速さか、どの箱か
module Main (main) where

import Control.DeepSeq   (force)
import Control.Exception (evaluate)
import Test.Tasty.Bench

import Boxes
import Frozen (buildSafe, buildUnsafe)
import Sieve
import Tally

-- ANCHOR: main
main :: IO ()
main = defaultMain
  [ env (evaluate (force (encounters 1000000))) $ \kinds ->
      bgroup "tally"
        [ bench "Map"    (nf tallyMap kinds)
        , bench "Vector" (nf tallyVector kinds)
        , bench "accum"  (nf tallyAccum kinds)
        ]
  , bgroup "sieve"
      [ bench "list 20000"     (nf primesList 20000)
      , bench "Vector 20000"   (nf primesVector 20000)
      , bench "Vector 1000000" (nf sieveVector 1000000)
      , bench "unsafe 1000000" (nf sieveUnsafe 1000000)
      -- A UArray has no NFData, but whnf is enough: it holds no thunks.
      -- UArray には NFData がないが、サンクを持たないので whnf で足りる
      , bench "array 1000000"  (whnf sieveArray 1000000)
      ]
  , bgroup "count"
      [ bench "modifyIORef'"       (nfIO (countStrict 1000000))
      , bench "atomicModifyIORef'" (nfIO (countAtomic 1000000))
      , bench "one-slot vector"    (nfIO (countSlot 1000000))
      ]
  , bgroup "freeze"
      [ bench "freeze"       (nf buildSafe 1000000)
      , bench "unsafeFreeze" (nf buildUnsafe 1000000)
      ]
  , env (evaluate (force [1 .. 1000000 :: Int])) $ \xs ->
      bgroup "sum"
        [ bench "State"  (nf sumState xs)
        , bench "ST"     (nf sumST xs)
        , bench "foldl'" (nf sumFold xs)
        ]
  ]
-- ANCHOR_END: main
-- ANCHOR_END: all
