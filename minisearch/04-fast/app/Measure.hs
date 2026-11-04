-- ANCHOR: all
-- | Build an index of a big library and search it, to measure.
--   Run with:  cabal run minisearch-measure -- 20000 +RTS -s
--   Add "naive" to measure the first, simple version, "strict" for
--   foldl' with the strict Map,
--   and "load" to also measure saving and loading.
-- 大きな書庫のインデックスを作って引き、測る。
-- "naive" を足すと最初の素朴な版を、"strict" で foldl' と正格な Map の版を、
-- "load" を足すと保存と読み込みも測る。
module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.Map as Map
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import Text.Printf (printf)

import MiniSearch.Corpus (corpus, queries)
import MiniSearch.Index (addDocument, buildIndex, emptyIndex)
import MiniSearch.Naive (buildIndexNaive, searchNaive)
import MiniSearch.Search (search)
import MiniSearch.Store (loadIndex, saveIndex)
import System.Directory (getFileSize, getTemporaryDirectory)
import System.FilePath ((</>))
import MiniSearch.Types

main :: IO ()
main = do
  args <- getArgs
  let n = case args of
            (a : _) -> read a
            []      -> 20000
      naive        = "naive" `elem` args
      build
        | naive               = buildIndexNaive
        | "strict" `elem` args = foldl' (flip addDocument) emptyIndex
        | otherwise           = buildIndex
      searchWith   = if naive then searchNaive else search
  -- Make the documents first, so that we measure only the index.
  -- 先に文書を作っておき、インデックスだけを測る
  docs <- evaluate (force (corpus n 100))

  t0    <- getMonotonicTime
  index <- evaluate (force (build docs))
  t1    <- getMonotonicTime
  -- 「文書の数と、単語の種類」
  printf "documents: %d, tokens: %d\n"
    (Map.size (indexDocs index)) (Map.size (indexPostings index))
  printf "build:  %.2f s\n" (t1 - t0)

  -- 1000 searches, keeping the top 10 of each (like the API).
  -- 1000 回の検索。それぞれ上位 10 件を取る（API と同じ）
  let top10 q = take 10 (searchWith index q)
  t2 <- getMonotonicTime
  total <- evaluate (sum (map (sum . map hitScore . top10) (queries 1000)))
  t3 <- getMonotonicTime
  printf "search: %.2f s (1000 queries, checksum %.3f)\n" (t3 - t2) total

  -- Save and load the index (what the server does at start):
  -- as JSON, and as binary.
  -- インデックスを保存して読む（サーバーが起動時にすること）：
  -- JSON と、バイナリーで
  if "load" `elem` args
    then do
      tmp <- getTemporaryDirectory
      mapM_ (saveAndLoad index tmp) ["index.json", "index.bin"]
    else pure ()

-- 💾 Save, then measure how long loading takes.
-- 保存してから、読むのにかかる時間を測る
saveAndLoad :: Index -> FilePath -> FilePath -> IO ()
saveAndLoad index tmp name = do
  let path = tmp </> name
  saveIndex path index
  size   <- getFileSize path
  t0     <- getMonotonicTime
  loaded <- loadIndex path >>= evaluate . force
  t1     <- getMonotonicTime
  case loaded of
    Left err -> putStrLn err
    Right i  -> printf "load %s: %.2f s (%d MB, same: %s)\n"
                  name (t1 - t0) (size `div` 1000000) (show (i == index))
-- ANCHOR_END: all
