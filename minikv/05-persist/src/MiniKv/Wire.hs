-- ANCHOR: all
-- | The wire format, modelled on Redis's RESP: every value says what it
--   is and how long it is, so a reader always knows where it ends.
-- 回線の上の形式（Redis の RESP が手本）：どの値も、何なのかと長さを
-- 先に言うので、読む側は、どこで終わるかがいつもわかる
module MiniKv.Wire
  ( Wire (..)
  , Parsed (..)
  , render
  , parseWire
  , Limits (..)
  , guestLimits
  , ownLimits
  , parseWireWithin
  , maxBulk
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

-- ANCHOR: wire
-- 📡 What travels on the wire.
-- 回線の上を流れるもの
data Wire
  = WSimple ByteString            -- ^ +OK
  | WError ByteString             -- ^ -ERR something went wrong
  | WInt Int                      -- ^ :42
  | WBulk (Maybe ByteString)      -- ^ $6 Lambda (Nothing is $-1, nil)
  | WArray [Wire]                 -- ^ *2 followed by 2 values
  deriving (Show, Eq)
-- ANCHOR_END: wire

-- ANCHOR: render
-- 🖨️ Write a value: a type mark, the length or the text, then \r\n.
-- 値を書く：種類の印、長さや中身、そして \r\n
render :: Wire -> ByteString
render = BL.toStrict . B.toLazyByteString . go
  where
    go (WSimple s)       = B.char7 '+' <> B.byteString s <> crlf
    go (WError s)        = B.char7 '-' <> B.byteString s <> crlf
    go (WInt n)          = B.char7 ':' <> B.intDec n <> crlf
    go (WBulk Nothing)   = B.string7 "$-1" <> crlf
    go (WBulk (Just s))  = B.char7 '$' <> B.intDec (BS.length s) <> crlf
                             <> B.byteString s <> crlf
    go (WArray items)    = B.char7 '*' <> B.intDec (length items) <> crlf
                             <> foldMap go items
    crlf = B.string7 "\r\n"
-- ANCHOR_END: render

-- ANCHOR: parsed
-- 📥 The three outcomes of reading from a buffer.
-- バッファーから読んだ結果の3通り
data Parsed a
  = NeedMore              -- ^ not all of it has arrived / まだ届いていない
  | Broken String         -- ^ it can never be read / 読めない
  | Parsed a ByteString   -- ^ the value, and what follows / 値と、その後ろ
  deriving (Show, Eq)

-- 🧱 The longest bulk string we accept (512 KB): a client cannot make
--    us wait for, or keep, a gigantic value.
-- 受け付ける文字列の長さの上限（512 KB）。巨大な値で待たせたり、
-- 抱えさせたりできないように
maxBulk :: Int
maxBulk = 512 * 1024
-- ANCHOR_END: parsed

-- ANCHOR: limits
-- 🚧 How much one value may hold. The limits protect us from guests;
--    what we wrote ourselves (the log) is read without them.
-- 値1つに、どこまで入ってよいか。上限は、外のお客さんから身を守るため。
-- 自分で書いたもの（ログ）は、上限なしで読む
data Limits = Limits
  { mostItems   :: Int   -- ^ items in one array / 配列1つの要素の数
  , longestBulk :: Int   -- ^ bytes in one bulk string / 文字列1つの長さ
  }

guestLimits :: Limits
guestLimits = Limits 1024 maxBulk

ownLimits :: Limits
ownLimits = Limits maxBound maxBound
-- ANCHOR_END: limits

-- ANCHOR: parse
-- 🔎 Read one value from the front of a buffer.
-- バッファーの先頭から、値を1つ読む
parseWire :: ByteString -> Parsed Wire
parseWire = parseWireWithin guestLimits

-- 🔎 The same, with the limits of the caller's choice.
-- 同じ読み方を、呼ぶ側が選んだ上限で
parseWireWithin :: Limits -> ByteString -> Parsed Wire
parseWireWithin limits input = case BC.uncons input of
  Nothing -> NeedMore
  Just (mark, rest) -> case mark of
    '+' -> withLine rest (\line after -> Parsed (WSimple line) after)
    '-' -> withLine rest (\line after -> Parsed (WError line) after)
    ':' -> withNumber rest (\n after -> Parsed (WInt n) after)
    '$' -> withNumber rest bulk
    '*' -> withNumber rest (\n after -> items n after [])
    c   -> Broken ("unknown type mark " <> show c)
  where
    bulk n after
      | n == -1        = Parsed (WBulk Nothing) after
      | n < 0          = Broken "negative length"
      | n > longestBulk limits = Broken "bulk string too long"
      | BS.length after - 2 < n = NeedMore
      | BS.take 2 (BS.drop n after) /= "\r\n" = Broken "missing \\r\\n"
      | otherwise      = Parsed (WBulk (Just (BS.take n after)))
                                (BS.drop (n + 2) after)
    items n after acc
      | n < 0 || n > mostItems limits = Broken "bad array length"
      | n == 0    = Parsed (WArray (reverse acc)) after
      | otherwise = case parseWireWithin limits after of
          Parsed item rest -> items (n - 1) rest (item : acc)
          NeedMore         -> NeedMore
          Broken why       -> Broken why

-- ✂️ The text up to \r\n, and what comes after it.
-- \r\n までの文字列と、その後ろ
withLine :: ByteString -> (ByteString -> ByteString -> Parsed a) -> Parsed a
withLine input k =
  let (line, rest) = BS.breakSubstring "\r\n" input
  in  if BS.length line > 64 * 1024 then Broken "line too long"
      else if BS.null rest then NeedMore
      else k line (BS.drop 2 rest)

-- 🔢 A whole number up to \r\n.
-- \r\n までの整数
withNumber :: ByteString -> (Int -> ByteString -> Parsed a) -> Parsed a
withNumber input k = withLine input $ \line after ->
  case BC.readInt line of
    Just (n, "") -> k n after
    _            -> Broken "not a number"
-- ANCHOR_END: parse
-- ANCHOR_END: all
