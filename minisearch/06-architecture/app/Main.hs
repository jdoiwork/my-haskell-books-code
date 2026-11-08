-- ANCHOR: head
-- ANCHOR: all
-- | minisearch: build an index, then search it.
-- minisearch：インデックスを作って、引く。
module Main (main) where

import qualified Data.Text.IO as TIO
import Options.Applicative (execParser)
import System.Exit (die)

import MiniSearch.Cli
import Data.Maybe (fromMaybe)

import MiniSearch.Index (buildIndexWith)
import MiniSearch.Load (readDocuments)
import MiniSearch.Report (formatResults)
import MiniSearch.Search (searchWith)
import MiniSearch.Store (loadIndex, saveIndex)
import MiniSearch.Tokenizer (english, tokenizerByName)
-- ANCHOR_END: head

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
  -- The language was already checked when reading the options.
  -- 言語は、オプションを読むときに確かめてある
  let tok   = fromMaybe english (tokenizerByName (indexLang opts))
      index = buildIndexWith tok docs
  saveIndex (indexOut opts) tok index
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
    -- The index remembers its tokenizer.
    -- インデックスは、自分のトークナイザーを覚えている
    Right (tok, index) -> do
      let hits = take (limit opts) (searchWith tok index (query opts))
      TIO.putStr (formatResults tok index (query opts) hits)
-- ANCHOR_END: runSearch
-- ANCHOR_END: all
