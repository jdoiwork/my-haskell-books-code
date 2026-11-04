-- ANCHOR: all
-- | Persistence: an append-only log of the commands that changed the
--   store, written in the wire format, and read back with the same
--   parser.
-- 永続化：ストアを変えたコマンドを、回線の形式で足していくだけのログ。
-- 読み戻しには、同じパーサーを使う
module MiniKv.Persist
  ( toLogged
  , encodeEntry
  , replay
  , Replayed (..)
  , rewriteLog
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T

import MiniKv.Protocol
import MiniKv.Store
import MiniKv.Types
import MiniKv.Wire

-- ANCHOR: logged
-- 📝 What goes into the log for a command run at `now`. Only commands
--    that change the store are kept; a relative EXPIRE becomes an
--    absolute PEXPIREAT, so that reading the log later gives the same
--    deadline.
-- 時刻 now に動かしたコマンドを、ログにどう書くか。ストアを変える
-- コマンドだけを残す。相対の EXPIRE は絶対の PEXPIREAT にする
-- （あとで読み戻しても、同じ期限になるように）
toLogged :: Millis -> Command -> Maybe Command
toLogged now command = case command of
  Expire key seconds -> Just (PExpireAt key (now + seconds * 1000))
  Get {}       -> Nothing
  Exists {}    -> Nothing
  LRange {}    -> Nothing
  LLen {}      -> Nothing
  SMembers {}  -> Nothing
  SIsMember {} -> Nothing
  HGet {}      -> Nothing
  HGetAll {}   -> Nothing
  TypeOf {}    -> Nothing
  Keys         -> Nothing
  Ttl {}       -> Nothing
  _            -> Just command

-- 💾 One log entry: the command, as an array of bulk strings.
-- ログの1件：コマンドを、文字列の配列として
encodeEntry :: Command -> ByteString
encodeEntry command =
  render (WArray [ WBulk (Just w) | w <- commandWords command ])
-- ANCHOR_END: logged

-- ANCHOR: replay
-- 📖 What reading a log gives back.
-- ログを読み戻した結果
data Replayed = Replayed
  { replayedStore :: Store
  , entries       :: Int            -- ^ how many were applied / 使った件数
  , problem       :: Maybe String   -- ^ why it stopped / 止まった理由
  }
  deriving (Show, Eq)

-- 🔁 Apply the entries one by one. A last entry that was cut short
--    (the server stopped while writing it) is left out, not an error.
-- 1件ずつ動かす。最後の1件が途中で切れていたら（書いている途中で
-- サーバーが止まったら）、それは使わない。エラーにはしない
replay :: ByteString -> Replayed
replay = go emptyStore 0
  where
    go store n bytes
      | BS.null bytes = Replayed store n Nothing
      | otherwise = case readRequest bytes of
          Parsed ws rest -> case toCommand ws of
            Right command -> go (snd (apply 0 command store)) (n + 1) rest
            Left err      -> Replayed store n (Just (T.unpack err))
          NeedMore   -> Replayed store n (Just "the last entry was cut short")
          Broken why -> Replayed store n (Just why)
-- ANCHOR_END: replay

-- ANCHOR: rewrite
-- 🗜️ A fresh, short log for a store: its snapshot, entry by entry.
-- ストアのための、新しく短いログ：スナップショットを1件ずつ
rewriteLog :: Store -> ByteString
rewriteLog store = foldMap encodeEntry (snapshot store)
-- ANCHOR_END: rewrite
-- ANCHOR_END: all
