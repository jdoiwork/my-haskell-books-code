-- ANCHOR: all
-- ANCHOR: head
-- | Persistence: an append-only log of the commands that changed the
--   store, written in the wire format, and read back with the same
--   parser.
-- 永続化：ストアを変えたコマンドを、回線の形式で足していくだけのログ。
-- 読み戻しには、同じパーサーを使う
module MiniKv.Persist
  ( toLogged
  , encodeEntry
  , logStep
  , sweepStep
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
-- ANCHOR_END: head

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

-- ANCHOR: step
-- 👣 Run a command at `now`, and say what to write in the log.
--    A command that only reads leaves the store as it was. When a
--    write forgets a key because its time has come, a DEL for it is
--    written first: reading the log back does not know what time it
--    was, so it would not forget the key by itself.
-- 時刻 now にコマンドを動かし、ログに何を書くかを答える。読むだけの
-- コマンドは、ストアを変えない。書くコマンドが、時刻が来たのでキーを
-- 忘れたら、先にそのキーの DEL を書く。読み戻すときには、そのときの
-- 時刻がわからないので、ひとりでには忘れてくれないから
logStep :: Millis -> Command -> Store -> ((Reply, [Command]), Store)
logStep now command store = case toLogged now command of
  Nothing     -> ((reply, []), store)
  Just logged -> ((reply, forgotten ++ [ logged | succeeded ]), store')
  where
    (reply, store') = apply now command store
    gone            = expiring now command store
    forgotten       = [ Del gone | not (null gone) ]
    succeeded       = case reply of
      Failed _ -> False
      _        -> True

-- 🧹 The sweeper's turn: forget the keys whose time has come, and
--    write them as one DEL. Says how many it forgot.
-- 掃除係の番：時刻が来たキーを忘れ、1つの DEL として書く。
-- いくつ忘れたかを答える
sweepStep :: Millis -> Store -> ((Int, [Command]), Store)
sweepStep now store =
  let gone        = dueKeys now store
      (n, store') = sweep now store
  in  ((n, [ Del gone | not (null gone) ]), store')
-- ANCHOR_END: step

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
    -- The bangs work out the store and the count at every step; without
    -- them, a long log piles up one unfinished sum per entry.
    -- ! は、ストアと件数を1件ごとに計算させる。ないと、長いログでは、
    -- 1件につき1つ、計算の途中がたまっていく
    go :: Store -> Int -> ByteString -> Replayed
    go !store !n bytes
      | BS.null bytes = Replayed store n Nothing
      | otherwise = case readEntry bytes of
          Parsed ws rest -> case toCommand ws of
            Right command -> go (snd (apply 0 command store)) (n + 1) rest
            Left err      -> Replayed store n (Just (T.unpack err))
          NeedMore   -> Replayed store n (Just "the last entry was cut short")
          Broken why -> Replayed store n (Just why)

-- 📜 One entry from the log: an array of bulk strings, read with the
--    parser of the wire format, but without the guests' limits. The
--    server may write an entry bigger than it would take from a guest
--    (a snapshot puts a whole list in one RPUSH).
-- ログから1件：文字列の配列。回線の形式のパーサーで読むが、お客さん用の
-- 上限は掛けない。サーバーは、お客さんからは受け取らない大きさの1件も
-- 書くから（スナップショットは、リストを丸ごと1つの RPUSH にする）
readEntry :: ByteString -> Parsed [ByteString]
readEntry bytes = case parseWireWithin ownLimits bytes of
  Parsed (WArray items) rest -> case traverse bulkText items of
    Just ws -> Parsed ws rest
    Nothing -> Broken "a log entry must be an array of bulk strings"
  Parsed _ _ -> Broken "a log entry must be an array"
  NeedMore   -> NeedMore
  Broken why -> Broken why
  where
    bulkText (WBulk (Just s)) = Just s
    bulkText _                = Nothing
-- ANCHOR_END: replay

-- ANCHOR: rewrite
-- 🗜️ A fresh, short log for a store: its snapshot, entry by entry.
-- ストアのための、新しく短いログ：スナップショットを1件ずつ
rewriteLog :: Store -> ByteString
rewriteLog store = foldMap encodeEntry (snapshot store)
-- ANCHOR_END: rewrite
-- ANCHOR_END: all
