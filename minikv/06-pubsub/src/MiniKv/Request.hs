-- ANCHOR: all
-- | What the server is asked: a plain command for the pure core, or
--   one of the requests only the server itself can handle (they wait,
--   or they change what the connection is doing).
-- サーバーが頼まれること：純粋な核に任せるコマンドか、サーバーにしか
-- 扱えない要求（待つ、つながりの役目を変える）か
-- ANCHOR: head
module MiniKv.Request
  ( Request (..)
  , toRequest
  , waitSeconds
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')

import MiniKv.Protocol (toCommand)
import MiniKv.Store (readInt)
import MiniKv.Types
-- ANCHOR_END: head

-- ANCHOR: request
-- 📮 The four kinds of requests. The type keeps the core pure: the core
--    never sees a request that waits or subscribes.
-- 4種類の要求。型が、核を純粋なままにしておく：
-- 待つ要求や購読する要求は、核には届かない
data Request
  = Plain Command        -- ^ for the pure core / 純粋な核に任せる
  | BLPop Key Int        -- ^ pop, waiting n seconds / n 秒まで待って取り出す
  | Publish Text Text    -- ^ send a message to a channel / チャンネルに送る
  | Subscribe [Text]     -- ^ listen to channels from now on / これから聞く
  deriving (Show, Eq)

-- 🧩 Words to a request.
-- 単語の列を要求に
toRequest :: [ByteString] -> Either Text Request
toRequest words' = case words' of
  name : args -> case (T.toUpper (T.pack (BC.unpack name)), args) of
    ("BLPOP",     [k, t])       -> BLPop <$> utf8 k <*> waitSeconds t
    ("PUBLISH",   [c, m])       -> Publish <$> utf8 c <*> utf8 m
    ("SUBSCRIBE", cs@(_ : _))   -> Subscribe <$> traverse utf8 cs
    _                           -> Plain <$> toCommand words'
  []          -> Plain <$> toCommand words'
  where
    utf8 bytes = either (const (Left "not UTF-8")) Right (decodeUtf8' bytes)

-- ⏲️ The seconds BLPOP may wait: digits only, read by the same `readInt`
--    as every other number. The server turns them into microseconds
--    (× 1000000), so a number that would go round the Int ring there
--    is refused here, at the gate.
-- BLPOP が待つ秒数：数字だけ。ほかの数と同じ readInt で読む。
-- サーバーは、これをマイクロ秒にする（× 1000000）。そこで Int の輪を
-- 回ってしまう数は、ここ（門）で断る
waitSeconds :: ByteString -> Either Text Int
waitSeconds bytes = case decodeUtf8' bytes of
  Right t | Just n <- readInt t, n >= 0 ->
    if n > maxBound `div` 1000000 then Left "the timeout is too long"
                                  else Right n
  _ -> Left "the timeout must be a whole number"
-- ANCHOR_END: request
-- ANCHOR_END: all
