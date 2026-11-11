-- ANCHOR: all
-- | A client that is strict about types: every key knows what it holds,
--   and every command knows what its answer is.
-- 型に厳しいクライアント：どのキーも、何をしまうかを知っていて、
-- どのコマンドも、答えが何かを知っている
-- ANCHOR: head
module MiniKv.Typed
  ( Key (..)
  , ListKey (..)
  , Codec (..)
  , Cmd (..)
  , ClientError (..)
  , run
  , runMany
  , cmdWords
  , decodeReply
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)

import MiniKv.Client (Connection, request, requestMany)
import MiniKv.Store (readInt)
import MiniKv.Wire
-- ANCHOR_END: head

-- ANCHOR: keys
-- 🔑 A key that holds a value of type a (Key Int holds numbers).
--    The type is only a promise we make on the client side.
-- 型 a の値をしまうキー（Key Int には数をしまう）。
-- 型は、クライアントの側での約束
newtype Key a = Key Text

-- 📜 A key that holds a list of a.
-- a のリストをしまうキー
newtype ListKey a = ListKey Text

-- 🔄 How a value travels as text, and comes back.
-- 値を文字列にして送り、文字列から戻す方法
class Codec a where
  encode :: a -> Text
  decode :: Text -> Maybe a

instance Codec Text where
  encode = id
  decode = Just

-- The same gate as the server's INCR: digits only, and within Int.
-- サーバーの INCR と同じ門番：数字だけで、Int に入るもの
instance Codec Int where
  encode = T.pack . show
  decode = readInt
-- ANCHOR_END: keys

-- ANCHOR: cmd
-- 📮 Commands, as a GADT: the type parameter is the type of the answer.
--    `Get k` answers Maybe v; `Incr k` answers Int; `Set k v` answers ().
-- コマンドを GADT で：型の引数が、答えの型。
-- Get k の答えは Maybe v、Incr k は Int、Set k v は ()
data Cmd a where
  Get    :: Codec v => Key v -> Cmd (Maybe v)
  Set    :: Codec v => Key v -> v -> Cmd ()
  Incr   :: Key Int -> Cmd Int
  RPush  :: Codec v => ListKey v -> [v] -> Cmd Int
  LRange :: Codec v => ListKey v -> Int -> Int -> Cmd [v]
  Expire :: Key v -> Int -> Cmd Bool
  Del    :: Key v -> Cmd Bool
-- ANCHOR_END: cmd

-- ANCHOR: words
-- 🔤 A command as the words on the wire.
-- コマンドを、回線の上の単語に
cmdWords :: Cmd a -> [ByteString]
cmdWords command = map encodeUtf8 $ case command of
  Get (Key k)              -> ["GET", k]
  Set (Key k) v            -> ["SET", k, encode v]
  Incr (Key k)             -> ["INCR", k]
  RPush (ListKey k) vs     -> "RPUSH" : k : map encode vs
  LRange (ListKey k) a b   -> ["LRANGE", k, showT a, showT b]
  Expire (Key k) seconds   -> ["EXPIRE", k, showT seconds]
  Del (Key k)              -> ["DEL", k]
  where
    showT n = T.pack (show n)
-- ANCHOR_END: words

-- ANCHOR: decode
-- ❌ What can go wrong on the client side.
-- クライアントの側で、うまくいかないこと
data ClientError
  -- the server answered with an error / サーバーがエラーを答えた
  = ServerSaid Text
  -- a value of the wrong shape / 形のちがう値
  | NotDecodable Text
  -- a reply we did not expect / 思っていない答え
  | Unexpected Wire
  deriving (Show, Eq)

-- 📬 Read the reply as the type the command promised. In each case,
--    GHC knows what `a` is (that is what a GADT gives us).
-- 答えを、コマンドが約束した型として読む。それぞれの場合で、
-- GHC は a が何かを知っている（それが GADT のくれるもの）
decodeReply :: Cmd a -> Wire -> Either ClientError a
decodeReply command reply = case (command, reply) of
  (_,          WError message) -> Left (ServerSaid (text message))
  (Get _,      WBulk Nothing)  -> Right Nothing
  (Get _,      WBulk (Just b)) -> Just <$> value b
  (Set _ _,    WSimple "OK")   -> Right ()
  (Incr _,     WInt n)         -> Right n
  (RPush _ _,  WInt n)         -> Right n
  (LRange {},  WArray items)   -> traverse item items
  (Expire _ _, WInt n)         -> Right (n == 1)
  (Del _,      WInt n)         -> Right (n == 1)
  _                            -> Left (Unexpected reply)
  where
    text b = either (const "?") id (decodeUtf8' b)
    value :: Codec v => ByteString -> Either ClientError v
    value b = maybe (Left (NotDecodable (text b))) Right (decode (text b))
    item (WBulk (Just b)) = value b
    item other            = Left (Unexpected other)

-- ▶️ Send a command, and get its answer as the promised type.
-- コマンドを送り、約束の型で答えを受け取る
run :: Connection -> Cmd a -> IO (Either ClientError a)
run conn command = decodeReply command <$> request conn (cmdWords command)
-- ANCHOR_END: decode

-- ANCHOR: runMany
-- 🚚 Many commands of one answer type in one pipeline: each reply is
--    read as its own command promised.
-- 答えの型が同じコマンドを、まとめてパイプラインで。答えは1つずつ、
-- そのコマンドが約束した型で読む
runMany :: Connection -> [Cmd a] -> IO [Either ClientError a]
runMany conn commands =
  zipWith decodeReply commands <$> requestMany conn (map cmdWords commands)
-- ANCHOR_END: runMany
-- ANCHOR_END: all

