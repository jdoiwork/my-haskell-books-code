-- ANCHOR: all
-- | The minikv server, with a log.
--   cabal run minikv-server -- 6380 minikv.log
-- ログ付きの minikv のサーバー
module Main (main) where

import Control.Concurrent.Async (link, withAsync)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (BufferMode (..), hSetBuffering, stdout)

import MiniKv.Persist (Replayed (..))
import MiniKv.Server

main :: IO ()
main = do
  args <- getArgs
  port <- case args of
    -- 「使い方：minikv-server [ポート 1〜65535]」
    (p : _) -> maybe (die ("usage: minikv-server [port 1-65535], not " <> p))
                     pure (readPort p)
    []      -> pure 6380
  let logPath = case args of
        (_ : path : _) -> path
        _              -> "minikv.log"
  hSetBuffering stdout LineBuffering
  (env, replayed) <- openEnv systemClock (Just logPath)
  -- 「ログから N 件を読み戻しました」
  putStrLn ("replayed " <> show (entries replayed) <> " entries from "
            <> logPath <> maybe "" (", stopped: " <>) (problem replayed))
  server <- listenOn port
  actual <- socketPort server
  -- 「minikv が待ち受けています」
  putStrLn ("minikv is listening on 127.0.0.1:" <> show actual)
  withGuests $ \welcome ->
    withAsync (sweeper env) $ \sweeping -> do
      link sweeping
      acceptLoop env server welcome
-- ANCHOR_END: all
