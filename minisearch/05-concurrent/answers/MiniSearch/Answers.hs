-- ANCHOR: all
-- ANCHOR: head
-- | Answers to the training of the concurrency chapter.
-- 並行化する章のトレーニングの解答。
module MiniSearch.Answers
  ( readDocumentsConcurrently
  , reportingSource
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM
import Control.Exception (IOException, displayException, throwIO, try)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (listDirectory)
import System.FilePath (takeExtension)

import MiniSearch.Load (readDocument)
import MiniSearch.Types
-- ANCHOR_END: head

-- ANCHOR: concurrently
-- 📂 Read the files at the same time (good when they are on a slow
--    disk or over the network). Each id comes from its own file,
--    so it does not matter which file is read first.
-- ファイルを同時に読む（遅いディスクやネットワークの先にあるとき
-- 効く）。番号はそれぞれのファイルから決まるので、
-- どのファイルが先に読み終わっても同じ
readDocumentsConcurrently :: FilePath -> IO [Document]
readDocumentsConcurrently dir = do
  names <- listDirectory dir
  let txtFiles = sort (filter (\n -> takeExtension n == ".txt") names)
  mapConcurrently (readDocument dir) txtFiles
-- ANCHOR_END: concurrently

-- ANCHOR: reporting
-- 📋 Wrap the source of documents, and remember how the last read
--    went: Nothing = it worked, Just message = it failed. The error
--    is thrown again, so the rebuild stops and the old index stays.
-- 文書の出どころを包んで、最後に読んだときの結果を覚えておく：
-- Nothing なら成功、Just メッセージなら失敗。エラーは投げ直すので、
-- 作り直しは止まり、古いインデックスが残る
reportingSource :: TVar (Maybe Text) -> IO [Document] -> IO [Document]
reportingSource status source = do
  result <- try source
  case result of
    Left err -> do
      let message = T.pack (displayException (err :: IOException))
      atomically (writeTVar status (Just message))
      throwIO err
    Right docs -> do
      atomically (writeTVar status Nothing)
      pure docs
-- ANCHOR_END: reporting
-- ANCHOR_END: all
