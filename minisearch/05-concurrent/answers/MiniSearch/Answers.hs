-- ANCHOR: all
-- ANCHOR: head
-- | Answers to the training of the concurrency chapter.
-- 並行化する章のトレーニングの解答。
module MiniSearch.Answers (readDocumentsConcurrently) where

import Control.Concurrent.Async (mapConcurrently)
import Data.List (sort)
import qualified Data.Text.IO.Utf8 as Utf8
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

import MiniSearch.Load (toDocument)
import MiniSearch.Types
-- ANCHOR_END: head

-- ANCHOR: concurrently
-- 📂 Read the files at the same time (good when they are on a slow
--    disk or over the network). The numbers stay in name order.
-- ファイルを同時に読む（遅いディスクやネットワークの先にあるとき
-- 効く）。番号は、名前の順のまま
readDocumentsConcurrently :: FilePath -> IO [Document]
readDocumentsConcurrently dir = do
  names <- listDirectory dir
  let txtFiles = sort (filter (\n -> takeExtension n == ".txt") names)
  contents <- mapConcurrently (\n -> Utf8.readFile (dir </> n)) txtFiles
  pure (zipWith toDocument [1 ..] contents)
-- ANCHOR_END: concurrently
-- ANCHOR_END: all
