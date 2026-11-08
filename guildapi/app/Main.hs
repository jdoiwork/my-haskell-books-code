-- ANCHOR: all
-- 🌐 Start the guild's API server.
--    GUILD_STORE chooses the store: "memory" or "sqlite:FILE".
-- ギルドの API サーバーを立ち上げる。
-- 置き場は GUILD_STORE で選ぶ："memory" か "sqlite:ファイル"
-- ANCHOR: imports
module Main (main) where

import Data.List                            (stripPrefix)
import Data.Maybe                           (fromMaybe)
import Network.Wai.Handler.Warp             (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Environment                   (lookupEnv)
import Text.Read                            (readMaybe)

import Guild.Server (app)
import Guild.Store  (newMemoryStore, withSqliteStore)
-- ANCHOR_END: imports

-- ANCHOR: main
main :: IO ()
main = do
  setting <- fromMaybe "sqlite:guild.db" <$> lookupEnv "GUILD_STORE"
  port    <- maybe 8080 id . (>>= readMaybe) <$> lookupEnv "PORT"
  -- 「http://localhost:8080 で待っています」
  putStrLn ("Listening on http://localhost:" ++ show port)
  case stripPrefix "sqlite:" setting of
    Just path -> withSqliteStore path (run port . logStdoutDev . app)
    Nothing   -> newMemoryStore >>= run port . logStdoutDev . app
-- ANCHOR_END: main
-- ANCHOR_END: all
