-- ANCHOR: all
-- | The minikv server.   cabal run minikv-server -- 6380
-- minikv のサーバー
module Main (main) where

import Control.Concurrent.STM (newTVarIO)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (BufferMode (..), hSetBuffering, stdout)

import MiniKv.Server
import MiniKv.Store

main :: IO ()
main = do
  args <- getArgs
  port <- case args of
    -- 「使い方：minikv-server [ポート 1〜65535]」
    (p : _) -> maybe (die ("usage: minikv-server [port 1-65535], not " <> p))
                     pure (readPort p)
    []      -> pure 6380
  hSetBuffering stdout LineBuffering
  storeVar <- newTVarIO emptyStore
  server <- listenOn port
  actual <- socketPort server
  -- 「minikv が待ち受けています」
  putStrLn ("minikv is listening on 127.0.0.1:" <> show actual)
  acceptLoop storeVar server
-- ANCHOR_END: all
