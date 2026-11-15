-- ANCHOR: all
-- 🌐 Start the guild's API server.
--    GUILD_STORE chooses the store: "memory" or "sqlite:FILE".
-- ギルドの API サーバーを立ち上げる。
-- 置き場は GUILD_STORE で選ぶ："memory" か "sqlite:ファイル"
-- ANCHOR: imports
module Main (main) where

import Data.Char                            (isDigit)
import Data.List                            (stripPrefix)
import Data.Maybe                           (fromMaybe)
import Network.Wai.Handler.Warp             (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Environment                   (lookupEnv)
import System.Exit                          (die)
import Text.Read                            (readMaybe)

import Guild.Server (app)
import Guild.Store  (newMemoryStore, withSqliteStore)
-- ANCHOR_END: imports

-- ANCHOR: main
main :: IO ()
main = do
  setting <- fromMaybe "sqlite:guild.db" <$> lookupEnv "GUILD_STORE"
  portEnv <- lookupEnv "PORT"
  port    <- case portEnv of
    Nothing -> pure 8080
    Just s  -> case readPort s of
      Just n  -> pure n
      -- 「PORT は 1〜65535 の数にしてください」
      Nothing -> die ("PORT must be a number from 1 to 65535: " ++ s)
  -- 「http://localhost:8080 で待っています」
  putStrLn ("Listening on http://localhost:" ++ show port)
  case stripPrefix "sqlite:" setting of
    Just path -> withSqliteStore path (run port . logStdoutDev . app)
    Nothing   -> newMemoryStore >>= run port . logStdoutDev . app

-- 🔢 A port: digits only, from 1 to 65535. readMaybe alone reads
--    "0x1F90" as 8080, and wraps a number too big for Int around.
-- ポート：数字だけで、1〜65535。readMaybe だけだと "0x1F90" を 8080 と読み、
-- Int に入らない数を、輪を回して別の数にする
readPort :: String -> Maybe Int
readPort s
  | not (null s), all isDigit s, length s <= 5
  , Just n <- readMaybe s, n >= 1, n <= 65535 = Just n
  | otherwise                                 = Nothing
-- ANCHOR_END: main
-- ANCHOR_END: all
