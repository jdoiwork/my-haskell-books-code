-- ANCHOR: all
-- | The minikv server.   cabal run minikv-server -- 6380
-- minikv のサーバー
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTVarIO)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Read (readMaybe)

import MiniKv.Server
import MiniKv.Store

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
        (p : _) | Just n <- readMaybe p -> n
        _                                -> 6380
  hSetBuffering stdout LineBuffering
  storeVar <- newTVarIO emptyStore
  server <- listenOn (fromInteger port)
  -- 「minikv が待ち受けています」
  putStrLn ("minikv is listening on 127.0.0.1:" <> show port)
  _ <- forkIO (sweeper systemClock storeVar)
  acceptLoop systemClock storeVar server
-- ANCHOR_END: all
