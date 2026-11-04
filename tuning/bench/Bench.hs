-- ANCHOR: all
-- 🏁 Benchmarks: the drafts against the faster versions.
-- ベンチマーク：下書きと、速くした版を比べる
module Main (main) where

import Data.Aeson      (encode)
import Test.Tasty.Bench

import Report
import TuningAnswers

-- ANCHOR: main
main :: IO ()
main = defaultMain
  [ bgroup "heroes"
      [ bench "nub" (nf heroesSlow battleLog)
      , bench "Set" (nf heroesFast battleLog)
      ]
  , bgroup "totals"
      [ bench "list" (nf totalsSlow battleLog)
      , bench "Map"  (nf totalsFast battleLog)
      ]
  , bgroup "json"
      [ bench "toJSON"     (nf encode monsters)
      , bench "toEncoding" (nf encode monstersFast)
      ]
  -- answers for the training / トレーニングの解答
  , bgroup "answers"
      [ bench "sorted" (nf totalsSorted battleLog)
      ]
  ]
-- ANCHOR_END: main
-- ANCHOR_END: all
