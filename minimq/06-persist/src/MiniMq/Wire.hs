-- ANCHOR: all
-- | Things that travel on the wire: how to write them, how to read them,
--   and a connection that remembers what it has read too far.
-- 回線を通るもの：どう書くか、どう読むか。それと、読みすぎた分を
-- 覚えておくつながり
module MiniMq.Wire
  ( Wire (..)
  , Conn
  , newConn
  , connSocket
  , send
  , Received (..)
  , receive
  , word
  , restOfLine
  , sized
  , textB
  , sizedB
  ) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteString, intDec,
                                toLazyByteString)
import Data.IORef
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv)
import qualified Network.Socket.ByteString.Lazy as Lazy

-- ANCHOR: class
-- 🔌 A thing that can travel: it knows how to be written, and how to be
--    read back. The tests check that reading what was written gives
--    the same thing back (for values that keep the rule of the wire).
-- 回線を通れるもの：書き方と、読み戻し方を知っている。
-- 「書いたものを読むと、元に戻る」ことを、テストで確かめる
-- （手順書きの決まりを守った値なら）
class Wire a where
  render :: a -> Builder
  parser :: Parser a
-- ANCHOR_END: class

-- ANCHOR: pieces
-- 🔤 A word: everything up to the next space or newline.
-- 単語：次の空白か改行までの全部
word :: Parser Text
word = decodeUtf8Lenient <$> A.takeWhile (\c -> c /= ' ' && c /= '\n')

-- 📜 The rest of the line, spaces and all.
-- 行の残り全部（空白も含めて）
restOfLine :: Parser Text
restOfLine = decodeUtf8Lenient <$> A.takeTill (== '\n')

-- 📏 A body with its length in front: "5\nhello". The length lets the
--    body hold spaces, newlines, anything.
-- 長さを先に書いた本文：「5\nhello」。長さがわかるので、本文には
-- 空白でも改行でも、何でも入れられる
sized :: Parser Text
sized = do
  n <- A.decimal <* A.char '\n'
  decodeUtf8Lenient <$> A.take n

textB :: Text -> Builder
textB = byteString . encodeUtf8

sizedB :: Text -> Builder
sizedB text = intDec (BS.length bytes) <> "\n" <> byteString bytes
  where
    bytes = encodeUtf8 text
-- ANCHOR_END: pieces

-- ANCHOR: conn
-- 🧵 A socket, and the bytes we read past the last frame.
-- ソケットと、最後のひとまとまりより先まで読んでしまったバイト
data Conn = Conn
  { connSocket :: Socket
  , leftover   :: IORef ByteString
  }

newConn :: Socket -> IO Conn
newConn sock = Conn sock <$> newIORef BS.empty

send :: Wire a => Conn -> a -> IO ()
send conn = Lazy.sendAll (connSocket conn) . toLazyByteString . render

-- 📨 What came in: one thing, the end, or a line we cannot read.
-- 届いたもの：1つ、おしまい、読めない行
data Received a = Got a | Closed | Garbled Text
  deriving (Show, Eq)

-- 📖 Read one thing. Attoparsec asks for more bytes by itself when
--    a frame is cut in the middle; an empty chunk means the other side
--    hung up.
-- 1つ読む。ひとまとまりが途中で切れていたら、attoparsec が自分で
-- 続きのバイトを頼む。空のかたまりは、相手が切ったという意味
receive :: Wire a => Conn -> IO (Received a)
receive conn = do
  buffer <- readIORef (leftover conn)
  result <- A.parseWith (recv (connSocket conn) 4096) parser buffer
  case result of
    A.Done rest a -> Got a <$ writeIORef (leftover conn) rest
    A.Fail rest _ _
      | BS.null rest -> pure Closed
      | otherwise    -> pure (Garbled (firstLine rest))
    A.Partial _ -> pure Closed
  where
    firstLine = decodeUtf8Lenient . BS.takeWhile (/= 10)
-- ANCHOR_END: conn
-- ANCHOR_END: all
