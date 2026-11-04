-- ANCHOR: all
-- | Benchmarks: the naive version against the fast one.
--   Run with:  cabal bench
-- ベンチマーク：素朴な版と、速い版を比べる。
module Main (main) where

import Test.Tasty.Bench

import MiniSearch.Corpus (corpus, queries)
import MiniSearch.Index (addDocument, buildIndex, emptyIndex)
import MiniSearch.Naive (buildIndexNaive, searchNaive)
import MiniSearch.Search (search)

main :: IO ()
main = do
  let docs  = corpus 2000 100
      index = buildIndex docs
      qs    = queries 100
      top10 f q = take 10 (f index q)
  defaultMain
    -- 「インデックスを作る（2000 文書）」
    [ bgroup "build 2000 documents"
        [ bench "naive: foldr, lazy Map"   $ nf buildIndexNaive docs
        , bench "foldl', strict Map"       $
            nf (foldl' (flip addDocument) emptyIndex) docs
        , bench "fast: all at once"        $ nf buildIndex docs
        ]
    -- 「検索する（100 回、それぞれ上位 10 件）」
    , bgroup "search 100 queries (top 10)"
        [ bench "naive" $ nf (map (top10 searchNaive)) qs
        , bench "fast"  $ nf (map (top10 search)) qs
        ]
    ]
-- ANCHOR_END: all
