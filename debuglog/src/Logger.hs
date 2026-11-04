{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- 📜 A small logger: levels, time stamps, and standard error.
-- 小さなロガー：ログレベル、時刻、標準エラー
module Logger
  ( LogLevel (..)
  , Logger (..)
  , stderrLogger
  , logDebug
  , logInfo
  , logWarn
  , logError
  , levelFromEnv
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.Environment (lookupEnv)
import System.IO (stderr)
import Text.Read (readMaybe)

-- ANCHOR: level
-- 🚦 How important a message is, from the least to the most.
-- メッセージの大事さ。小さいものから大きいものの順
data LogLevel = Debug | Info | Warn | Error
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
-- ANCHOR_END: level

-- ANCHOR: logger
-- 📜 A logger is just a function: "log this text at this level".
-- ロガーは、ただの関数：「このレベルで、この文を記録して」
newtype Logger = Logger
  { logAt :: LogLevel -> Text -> IO ()
  }

-- 🖨️ A logger that writes to standard error, with the time,
--    and skips messages below the minimum level.
-- 標準エラーに時刻付きで書くロガー
-- 最低レベルより小さいメッセージは書かない
stderrLogger :: LogLevel -> Logger
stderrLogger minLevel = Logger write
  where
    write level msg
      | level < minLevel = pure ()
      | otherwise = do
          now <- getCurrentTime
          let stamp = T.pack (formatTime defaultTimeLocale "%H:%M:%S" now)
              line  = stamp <> " [" <> T.pack (show level) <> "] " <> msg
          TIO.hPutStrLn stderr line
-- ANCHOR_END: logger

-- ANCHOR: helpers
-- ✏️ Short names for each level.
-- レベルごとの短い名前
logDebug, logInfo, logWarn, logError :: Logger -> Text -> IO ()
logDebug logger = logAt logger Debug
logInfo  logger = logAt logger Info
logWarn  logger = logAt logger Warn
logError logger = logAt logger Error
-- ANCHOR_END: helpers

-- ANCHOR: env
-- 🌱 Read the minimum level from the LOG_LEVEL environment variable.
--    If it is not set, or not a level, use Info.
-- 環境変数 LOG_LEVEL から最低レベルを読む
-- 設定がない、またはレベルとして読めなければ Info
levelFromEnv :: IO LogLevel
levelFromEnv = do
  value <- lookupEnv "LOG_LEVEL"
  case value of
    Nothing   -> pure Info
    Just text ->
      case readMaybe text of
        Just level -> pure level
        Nothing    -> pure Info
-- ANCHOR_END: env
-- ANCHOR_END: all
