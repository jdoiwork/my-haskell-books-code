-- ANCHOR: all
-- | Start the web app. Everything it needs comes from environment
--   variables, so the same program runs on your machine and in Docker.
-- Web アプリを起動する。必要なものは全部環境変数から受け取るので、
-- 手元でも Docker の中でも、同じプログラムが動く
module Main (main) where

import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Environment (lookupEnv)
import System.IO (BufferMode (..), hSetBuffering, stdout)

import Board.Server (app)
import Board.Store (memoryStore, postgresStore)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  port   <- maybe 8080 read <$> lookupEnv "PORT"
  static <- fromMaybe "frontend/dist" <$> lookupEnv "STATIC_DIR"
  -- DATABASE_URL があれば PostgreSQL、なければメモリー
  store  <- lookupEnv "DATABASE_URL" >>= maybe memoryStore postgresStore
  -- 「http://localhost:8080 で待っています」
  putStrLn ("listening on http://localhost:" <> show port)
  run port (logStdout (app store static))
-- ANCHOR_END: all
