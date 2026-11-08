-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}

-- ANCHOR: module
module JsonLog where

-- JSON.
-- JSON
import Data.Aeson
  ( Options (..)
  , ToJSON (..)
  , camelTo2
  , defaultOptions
  , encode
  , genericToJSON
  , object
  , (.=)
  )
import Data.Aeson.Types (Pair)
import GHC.Generics     (Generic)

-- Bytes, text, the clock, and the standard error.
-- バイト列、テキスト、時計、標準エラー
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text       (Text)
import Data.Time       (UTCTime, getCurrentTime)
import System.IO       (stderr)
-- ANCHOR_END: imports
-- ANCHOR_END: module

-- ANCHOR: level
-- 🚦 How important a message is (from the chapter on debugging).
--    In JSON, it is a lowercase string such as "info".
-- メッセージの重要度（デバッグとログの章と同じ）
-- JSON では "info" のような小文字の文字列にする
data LogLevel = Debug | Info | Warn | Error
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON LogLevel where
  toJSON = genericToJSON defaultOptions
    { constructorTagModifier = camelTo2 '_' }
-- ANCHOR_END: level

-- ANCHOR: logLine
-- 📝 One log line as JSON: the time, the level, the message,
--    and any other fields we want to add.
-- 1行の JSON のログ：時刻、レベル、メッセージ、そして好きなフィールド
logLine :: UTCTime -> LogLevel -> Text -> [Pair] -> BL.ByteString
logLine time level message fields =
  let common = [ "time"  .= time
               , "level" .= level
               , "msg"   .= message
               ]
  in  encode (object (common ++ fields))

-- 📢 Write a JSON log line to the standard error.
-- JSON のログを1行、標準エラーに書く
logJson :: LogLevel -> Text -> [Pair] -> IO ()
logJson level message fields = do
  now <- getCurrentTime
  BL.hPutStrLn stderr (logLine now level message fields)
-- ANCHOR_END: logLine
-- ANCHOR_END: all
