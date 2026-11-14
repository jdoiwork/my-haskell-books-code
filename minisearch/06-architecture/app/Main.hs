-- ANCHOR: head
-- ANCHOR: all
-- | minisearch: build an index, then search it.
-- minisearch：インデックスを作って、引く。
module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative (execParser)
import System.Exit (die)

import MiniSearch.Cli

import MiniSearch.Index (buildIndexWith)
import MiniSearch.Load (readDocuments)
import MiniSearch.Report (formatResults)
import MiniSearch.Search (searchWith)
import MiniSearch.Store (loadIndex, saveIndex)
import MiniSearch.Tokenizer (tokenizerByName)
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
  -- The options carry the name; look the Tokenizer up again here.
  -- オプションは名前を運ぶので、ここで Tokenizer を引き直す
  tok <- case tokenizerByName (indexLang opts) of
    Just t  -> pure t
    -- 「知らない言語です」（オプションで確かめてあるので、ふつうは来ない）
    Nothing -> die ("unknown language: " <> T.unpack (indexLang opts))
  let index = buildIndexWith tok docs
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
