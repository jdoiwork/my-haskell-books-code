-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | minisearch-server: serve a live index over HTTP.
-- minisearch-server：生きているインデックスを HTTP で公開する。
module Main (main) where

import qualified Data.Text as T
import Network.Wai.Handler.Warp
  (Settings, defaultSettings, runSettings, setBeforeMainLoop, setHost,
   setPort)
import System.Environment (getEnvironment)
import System.Exit (die)

import MiniSearch.Api (app)
import MiniSearch.Config
import MiniSearch.Live (newLive)
import MiniSearch.Load (readDocuments)
import MiniSearch.Tokenizer (tokenizerName)

main :: IO ()
main = do
  env <- getEnvironment
  case loadConfig env of
    -- 「設定がまちがっています：……」
    Left err  -> die ("bad setting: " <> err)
    Right cfg -> do
      live <- newLive (configTokenizer cfg)
                      (readDocuments (configDocs cfg))
      -- Say "Listening" only after the port is open.
      -- ポートを開いてから「待っています」と言う
      let language = T.unpack (tokenizerName (configTokenizer cfg))
          ready    = setBeforeMainLoop
            -- 「http://localhost:8080 で待っています（言語：en）」
            (putStrLn ("Listening on http://localhost:"
                       <> show (configPort cfg)
                       <> " (language: " <> language <> ")"))
            (onlyThisMachine (configPort cfg))
      runSettings ready (app live)
-- 🔒 Listen on 127.0.0.1 only: other machines on the same network
--    cannot come in.
-- 127.0.0.1 だけで待つ。同じネットワークのほかの機械からは入れない
onlyThisMachine :: Int -> Settings
onlyThisMachine port = setHost "127.0.0.1" (setPort port defaultSettings)
-- ANCHOR_END: all
