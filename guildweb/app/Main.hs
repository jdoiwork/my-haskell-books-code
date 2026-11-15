-- ANCHOR: all
-- | Start the web app. Everything it needs comes from environment
--   variables, so the same program runs on your machine and in Docker.
-- Web アプリを起動する。必要なものは全部環境変数から受け取るので、
-- 手元でも Docker の中でも、同じプログラムが動く
module Main (main) where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Read (readMaybe)

import Board.Server (app)
import Board.Store (memoryStore, postgresStore)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  port   <- lookupEnv "PORT" >>= maybe (pure 8080) portOrDie
  static <- fromMaybe "frontend/dist" <$> lookupEnv "STATIC_DIR"
  -- DATABASE_URL があれば PostgreSQL、なければメモリー
  store  <- lookupEnv "DATABASE_URL" >>= maybe memoryStore postgresStore
  -- 「http://localhost:8080 で待っています」
  putStrLn ("listening on http://localhost:" <> show port)
  run port (logStdout (app store static))

-- 🔢 A port: digits only, from 1 to 65535. Otherwise, stop with a message.
-- ポート：数字だけで、1〜65535。そうでなければ、わけを出して止まる
portOrDie :: String -> IO Int
portOrDie s
  | not (null s), all isDigit s, length s <= 5
  , Just n <- readMaybe s, n >= 1, n <= 65535 = pure n
  -- 「PORT は 1〜65535 の数にしてください」
  | otherwise = die ("PORT must be a number from 1 to 65535: " <> s)
-- ANCHOR_END: all
