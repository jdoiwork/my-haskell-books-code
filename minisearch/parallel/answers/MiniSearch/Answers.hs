-- ANCHOR: all
-- ANCHOR: head
-- | Answers to the training of the parallel chapter.
-- 並列にする章のトレーニングの解答。
module MiniSearch.Answers
  ( mergeTree
  , readDocumentsConcurrently
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.DeepSeq (NFData)
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.List (sort)
import qualified Data.Text.IO.Utf8 as Utf8
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

import MiniSearch.Load (toDocument)
import MiniSearch.Types
-- ANCHOR_END: head

-- ANCHOR: tree
-- 🏆 Put the pieces together like a tournament: pairs first, then
--    pairs of pairs, ... Each round puts its pairs together in
--    parallel. The answer is the same as mconcat, because <> is
--    associative.
-- トーナメントの形で合わせる：まず2つずつ、次はその2つずつ、……。
-- 各回の組は、並列に合わせる。<> が結合法則を満たすので、
-- 答えは mconcat と同じ
mergeTree :: (NFData a, Monoid a) => [a] -> a
mergeTree []  = mempty
mergeTree [x] = x
mergeTree xs  = mergeTree (pairUp xs `using` parList rdeepseq)
  where
    pairUp (a : b : rest) = (a <> b) : pairUp rest
    pairUp rest           = rest
-- ANCHOR_END: tree

-- ANCHOR: concurrently
-- 📂 Read the files at the same time (good when they are on a slow
--    disk or over the network). mapConcurrently gives the contents
--    back in the order of the names, so the numbers are 1, 2, 3, ...
--    in name order, as in readDocuments.
-- ファイルを同時に読む（遅いディスクやネットワークの先にあるとき
-- 効く）。mapConcurrently は、中身を名前の順に返すので、
-- 番号は readDocuments と同じく、名前の順に 1, 2, 3, ...
readDocumentsConcurrently :: FilePath -> IO [Document]
readDocumentsConcurrently dir = do
  names <- listDirectory dir
  let txtFiles = sort (filter (\n -> takeExtension n == ".txt") names)
  contents <- mapConcurrently (\n -> Utf8.readFile (dir </> n)) txtFiles
  pure (zipWith toDocument [1 ..] contents)
-- ANCHOR_END: concurrently
-- ANCHOR_END: all
