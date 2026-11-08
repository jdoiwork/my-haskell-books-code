-- ANCHOR: all
-- | Between the wire and the core: words to commands, replies to wire.
-- 回線と核のあいだ：単語の列をコマンドに、答えを回線の形式に
-- ANCHOR: head
module MiniKv.Protocol
  ( readRequest
  , toCommand
  , commandWords
  , replyWire
  , errorText
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)

import MiniKv.Types
import MiniKv.Wire
-- ANCHOR_END: head

-- ANCHOR: request
-- 📮 One request from the buffer: an array of bulk strings, or (to be
--    friendly to people typing in nc) one plain line of words.
-- バッファーから要求を1つ：文字列の配列か、（nc で打つ人のために）
-- 単語を並べた1行
readRequest :: ByteString -> Parsed [ByteString]
readRequest input
  | BC.take 1 input == "*" = case parseWire input of
      Parsed (WArray items) rest -> case traverse bulkText items of
        Just ws -> Parsed ws rest
        Nothing -> Broken "a request must be an array of bulk strings"
      Parsed _ _ -> Broken "a request must be an array"
      NeedMore   -> NeedMore
      Broken why -> Broken why
  | otherwise =
      let (line, rest) = BC.breakSubstring "\r\n" input
      in  if BC.null rest then NeedMore
                          else Parsed (BC.words line) (BC.drop 2 rest)
  where
    bulkText (WBulk (Just s)) = Just s
    bulkText _                = Nothing
-- ANCHOR_END: request

-- ANCHOR: toCommand
-- 🧩 Words to a command. The name is case-insensitive; the number of
--    arguments and the numbers in them are checked here.
-- 単語の列をコマンドに。名前の大文字と小文字は区別しない。
-- 引数の数と、数であるべき引数は、ここで確かめる
toCommand :: [ByteString] -> Either Text Command
toCommand [] = Left "empty request"
toCommand (name : rawArgs) = do
  args <- traverse utf8 rawArgs
  case (T.toUpper (T.pack (BC.unpack name)), args) of
    ("GET",       [k])            -> Right (Get k)
    ("SET",       [k, v])         -> Right (Set k v)
    ("DEL",       ks@(_ : _))     -> Right (Del ks)
    ("EXISTS",    [k])            -> Right (Exists k)
    ("INCR",      [k])            -> Right (Incr k)
    ("INCRBY",    [k, n])         -> IncrBy k <$> number n
    ("LPUSH",     k : xs@(_ : _)) -> Right (LPush k xs)
    ("RPUSH",     k : xs@(_ : _)) -> Right (RPush k xs)
    ("LPOP",      [k])            -> Right (LPop k)
    ("RPOP",      [k])            -> Right (RPop k)
    ("LRANGE",    [k, a, b])      -> LRange k <$> number a <*> number b
    ("LLEN",      [k])            -> Right (LLen k)
    ("SADD",      k : xs@(_ : _)) -> Right (SAdd k xs)
    ("SREM",      k : xs@(_ : _)) -> Right (SRem k xs)
    ("SMEMBERS",  [k])            -> Right (SMembers k)
    ("SISMEMBER", [k, x])         -> Right (SIsMember k x)
    ("HSET",      [k, f, v])      -> Right (HSet k f v)
    ("HGET",      [k, f])         -> Right (HGet k f)
    ("HGETALL",   [k])            -> Right (HGetAll k)
    ("TYPE",      [k])            -> Right (TypeOf k)
    ("KEYS",      [])             -> Right Keys
    (other,       _)              ->
      Left ("unknown command or wrong number of arguments: " <> other)
  where
    utf8 bytes = either (const (Left "not UTF-8")) Right (decodeUtf8' bytes)
    number t = case reads (T.unpack t) of
      [(n, "")] -> Right n
      _         -> Left ("not an integer: " <> t)
-- ANCHOR_END: toCommand

-- ANCHOR: commandWords
-- 🔁 The other way: a command to words (for clients and logs).
-- 逆向き：コマンドを単語の列に（クライアントとログのため）
commandWords :: Command -> [ByteString]
commandWords command = map encodeUtf8 $ case command of
  Get k           -> ["GET", k]
  Set k v         -> ["SET", k, v]
  Del ks          -> "DEL" : ks
  Exists k        -> ["EXISTS", k]
  Incr k          -> ["INCR", k]
  IncrBy k n      -> ["INCRBY", k, showT n]
  LPush k xs      -> "LPUSH" : k : xs
  RPush k xs      -> "RPUSH" : k : xs
  LPop k          -> ["LPOP", k]
  RPop k          -> ["RPOP", k]
  LRange k a b    -> ["LRANGE", k, showT a, showT b]
  LLen k          -> ["LLEN", k]
  SAdd k xs       -> "SADD" : k : xs
  SRem k xs       -> "SREM" : k : xs
  SMembers k      -> ["SMEMBERS", k]
  SIsMember k x   -> ["SISMEMBER", k, x]
  HSet k f v      -> ["HSET", k, f, v]
  HGet k f        -> ["HGET", k, f]
  HGetAll k       -> ["HGETALL", k]
  TypeOf k        -> ["TYPE", k]
  Keys            -> ["KEYS"]
  where
    showT n = T.pack (show n)
-- ANCHOR_END: commandWords

-- ANCHOR: reply
-- 📬 A reply on the wire.
-- 回線の上の答え
replyWire :: Reply -> Wire
replyWire reply = case reply of
  Ok         -> WSimple "OK"
  Nil        -> WBulk Nothing
  Bulk t     -> WBulk (Just (encodeUtf8 t))
  Number n   -> WInt n
  Many ts    -> WArray [ WBulk (Just (encodeUtf8 t)) | t <- ts ]
  Failed err -> WError (encodeUtf8 (errorText err))

-- 💬 Errors, in words. The first word is a code, as in Redis.
-- エラーを言葉に。Redis と同じく、最初の単語はエラーの種類
errorText :: KvError -> Text
errorText err = case err of
  WrongType k wanted found ->
    "WRONGTYPE " <> k <> " holds a " <> kindWord found
      <> ", not a " <> kindWord wanted
  NotAnInteger k -> "ERR the value of " <> k <> " is not an integer"
  Overflow k     -> "ERR increment would overflow " <> k
  where
    kindWord KString = "string"
    kindWord KList   = "list"
    kindWord KSet    = "set"
    kindWord KHash   = "hash"
-- ANCHOR_END: reply
-- ANCHOR_END: all
