-- ANCHOR: all
-- | minisearch-server: serve a live index over HTTP.
-- minisearch-server：生きているインデックスを HTTP で公開する。
module Main (main) where

import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)

import MiniSearch.Api (app)
import MiniSearch.Live (newLive)
import MiniSearch.Load (readDocuments)

main :: IO ()
main = do
  -- The folder of documents and the port can be set from outside.
  -- 文書のフォルダーとポートは、外から指定できる
  dir  <- maybe "samples/docs" id <$> lookupEnv "MINISEARCH_DOCS"
  port <- maybe 8080 read <$> lookupEnv "PORT"
  -- POST /reindex reads the folder again.
  -- POST /reindex で、フォルダーを読み直す
  live <- newLive (readDocuments dir)
  -- 「http://localhost:8080 で待っています」
  putStrLn ("Listening on http://localhost:" <> show port)
  run port (app live)
-- ANCHOR_END: all
