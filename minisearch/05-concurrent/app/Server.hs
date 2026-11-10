-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | minisearch-server: serve a live index over HTTP.
-- minisearch-server：生きているインデックスを HTTP で公開する。
module Main (main) where

import Network.Wai.Handler.Warp
  (Settings, defaultSettings, runSettings, setHost, setPort)
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
  runSettings (onlyThisMachine port) (app live)
-- 🔒 Listen on 127.0.0.1 only: other machines on the same network
--    cannot come in.
-- 127.0.0.1 だけで待つ。同じネットワークのほかの機械からは入れない
onlyThisMachine :: Int -> Settings
onlyThisMachine port = setHost "127.0.0.1" (setPort port defaultSettings)
-- ANCHOR_END: all
