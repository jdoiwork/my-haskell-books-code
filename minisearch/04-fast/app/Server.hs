-- ANCHOR: all
-- | minisearch-server: serve an index over HTTP.
-- minisearch-server：インデックスを HTTP で公開する。
module Main (main) where

import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)
import System.Exit (die)

import MiniSearch.Api (app)
import MiniSearch.Store (loadIndex)

main :: IO ()
main = do
  -- The index file and the port can be set from outside.
  -- インデックスのファイルとポートは、外から指定できる
  path <- maybe "index.json" id <$> lookupEnv "MINISEARCH_INDEX"
  port <- maybe 8080 read <$> lookupEnv "PORT"
  loaded <- loadIndex path
  case loaded of
    Left err    -> die ("cannot read " <> path <> ": " <> err)
    Right index -> do
      -- 「http://localhost:8080 で待っています」
      putStrLn ("Listening on http://localhost:" <> show port)
      run port (app index)
-- ANCHOR_END: all
