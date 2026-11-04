-- ANCHOR: all
-- | minisearch: build an index, then search it.
-- minisearch：インデックスを作って、引く。
module Main (main) where

import qualified Data.Text.IO as TIO
import Options.Applicative (execParser)
import System.Exit (die)

import MiniSearch.Cli
import MiniSearch.Index (buildIndex)
import MiniSearch.Load (readDocuments)
import MiniSearch.Report (formatResults)
import MiniSearch.Search (search)
import MiniSearch.Store (loadIndex, saveIndex)

main :: IO ()
main = do
  cmd <- execParser commandInfo
  case cmd of
    IndexCmd opts  -> runIndex opts
    SearchCmd opts -> runSearch opts

-- ANCHOR: runIndex
-- 🏗️ Read the folder, build the index, save it.
-- フォルダーを読み、インデックスを作り、保存する
runIndex :: IndexOptions -> IO ()
runIndex opts = do
  docs <- readDocuments (docsDir opts)
  let index = buildIndex docs
  saveIndex (indexOut opts) index
  -- 「8 個の文書を、index.json に入れました」
  putStrLn ("Indexed " <> show (length docs)
            <> " documents into " <> indexOut opts)
-- ANCHOR_END: runIndex

-- ANCHOR: runSearch
-- 🔍 Load the index, search, print.
-- インデックスを読み、検索して、表示する
runSearch :: SearchOptions -> IO ()
runSearch opts = do
  loaded <- loadIndex (indexIn opts)
  case loaded of
    -- 「index.json を読めません：……」
    Left err    -> die ("cannot read " <> indexIn opts <> ": " <> err)
    Right index -> do
      let hits = take (limit opts) (search index (query opts))
      TIO.putStr (formatResults index (query opts) hits)
-- ANCHOR_END: runSearch
-- ANCHOR_END: all
