-- ANCHOR: all
-- | The minikv server, with a log.
--   cabal run minikv-server -- 6380 minikv.log
-- ログ付きの minikv のサーバー
module Main (main) where

import Control.Concurrent (forkIO)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Read (readMaybe)

import MiniKv.Persist (Replayed (..))
import MiniKv.Server

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
        (p : _) | Just n <- readMaybe p -> n
        _                                -> 6380 :: Integer
      logPath = case args of
        (_ : path : _) -> path
        _              -> "minikv.log"
  hSetBuffering stdout LineBuffering
  (env, replayed) <- openEnv systemClock (Just logPath)
  -- 「ログから N 件を読み戻しました」
  putStrLn ("replayed " <> show (entries replayed) <> " entries from "
            <> logPath <> maybe "" (", stopped: " <>) (problem replayed))
  server <- listenOn (fromInteger port)
  -- 「minikv が待ち受けています」
  putStrLn ("minikv is listening on 127.0.0.1:" <> show port)
  _ <- forkIO (sweeper env)
  acceptLoop env server
-- ANCHOR_END: all
