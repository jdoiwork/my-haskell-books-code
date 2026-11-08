-- ANCHOR: head
-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | Read the documents from a folder.
-- フォルダーから文書を読む。
module MiniSearch.Load
  ( readDocuments
  , toDocument
  ) where

import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as Utf8
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

import MiniSearch.Types
-- ANCHOR_END: head

-- ANCHOR: toDocument
-- 📄 The first line is the title, the rest is the body.
-- 1行目が題、残りが本文
toDocument :: Int -> Text -> Document
toDocument n content =
  case T.lines content of
    []             -> Document (DocId n) "" ""
    (title : rest) -> Document (DocId n) (T.strip title) (T.unlines rest)
-- ANCHOR_END: toDocument

-- ANCHOR: read
-- 📂 Read every .txt file in the folder, in name order.
--    The numbers are 1, 2, 3, ... in that order.
-- フォルダーの .txt を全部、名前の順に読む。
-- 番号は、その順に 1, 2, 3, ...
readDocuments :: FilePath -> IO [Document]
readDocuments dir = do
  names <- listDirectory dir
  let txtFiles = sort (filter (\n -> takeExtension n == ".txt") names)
  contents <- mapM (\n -> Utf8.readFile (dir </> n)) txtFiles
  pure (zipWith toDocument [1 ..] contents)
-- ANCHOR_END: read
-- ANCHOR_END: all
