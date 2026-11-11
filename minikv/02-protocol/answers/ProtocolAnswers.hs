-- ANCHOR: all
-- | Answers to the training of "Protocol and parser".
-- 「プロトコルとパーサー」のトレーニングの解答
module ProtocolAnswers
  ( answerWords
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Char (toUpper)
import Data.Text.Encoding (encodeUtf8)

import MiniKv.Protocol
import MiniKv.Store
import MiniKv.Wire

-- ANCHOR: ping
-- 🏓 Step 2, the light way: answer PING where the request is read,
--    before the core sees it. The core and its types stay as they are.
-- ステップ2の軽いやり方：要求を読む所で、核に渡す前に PING に答える。
-- 核とその型は、そのまま
answerWords :: [ByteString] -> Store -> (Wire, Store)
answerWords [name] store
  | BC.map toUpper name == "PING" = (WSimple "PONG", store)
answerWords ws store = case toCommand ws of
  Left message  -> (WError ("ERR " <> encodeUtf8 message), store)
  Right command ->
    let (reply, store') = apply command store
    in  (replyWire reply, store')
-- ANCHOR_END: ping
-- ANCHOR_END: all
