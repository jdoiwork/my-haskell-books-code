-- ANCHOR: all
-- | minisearch-server: serve a live index over HTTP.
-- minisearch-server：生きているインデックスを HTTP で公開する。
module Main (main) where

import qualified Data.Text as T
import Network.Wai.Handler.Warp (run)
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
      -- 「http://localhost:8080 で待っています（言語：en）」
      putStrLn ("Listening on http://localhost:" <> show (configPort cfg)
                <> " (language: "
                <> T.unpack (tokenizerName (configTokenizer cfg)) <> ")")
      run (configPort cfg) (app live)
-- ANCHOR_END: all
