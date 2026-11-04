-- ANCHOR: all
-- | What the server is asked: a plain command for the pure core, or
--   one of the requests only the server itself can handle (they wait,
--   or they change what the connection is doing).
-- サーバーが頼まれること：純粋な核に任せるコマンドか、サーバーにしか
-- 扱えない要求（待つ、つながりの役目を変える）か
module MiniKv.Request
  ( Request (..)
  , toRequest
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')

import MiniKv.Protocol (toCommand)
import MiniKv.Types

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
    ("BLPOP",     [k, t])       -> BLPop <$> utf8 k <*> number t
    ("PUBLISH",   [c, m])       -> Publish <$> utf8 c <*> utf8 m
    ("SUBSCRIBE", cs@(_ : _))   -> Subscribe <$> traverse utf8 cs
    _                           -> Plain <$> toCommand words'
  []          -> Plain <$> toCommand words'
  where
    utf8 bytes = either (const (Left "not UTF-8")) Right (decodeUtf8' bytes)
    number bytes = case BC.readInt bytes of
      Just (n, "") | n >= 0 -> Right n
      _                     -> Left "the timeout must be a whole number"
-- ANCHOR_END: request
-- ANCHOR_END: all
