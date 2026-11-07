-- ANCHOR: all
-- | What clients ask, what the broker answers, and how both look on
--   the wire.
-- クライアントが頼むこと、ブローカーが答えること、
-- そして、その両方が回線の上でどう見えるか
module MiniMq.Protocol
  ( Request (..)
  , Response (..)
  , oneWord
  , brokenNames
  ) where

import Control.Applicative (optional)
import Data.Attoparsec.ByteString.Char8 (Parser, char, choice, decimal,
                                         option, string)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteString, intDec)
import Data.Coerce (Coercible, coerce)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T

import MiniMq.Types
import MiniMq.Wire

-- ANCHOR: request
-- 📮 Everything a client can ask.
-- クライアントが頼めること
data Request
  = DeclareQueue    QueueName QueueSettings
  | DeclareExchange ExchangeName ExchangeKind
  | Bind            ExchangeName RoutingKey QueueName
  | Publish         ExchangeName Message
  | Consume         QueueName Int          -- ^ new: how many to hold at once
  | Settle          Settlement Tag         -- ^ new: ACK, REQUEUE, REJECT
  deriving (Show, Eq)

-- 📬 Everything the broker says. DELIVER comes by itself, whenever
--    a letter is ready for a consumer.
-- ブローカーが言うこと。DELIVER は、受け手に渡せる手紙ができたら、
-- 頼まれなくても届く
data Response
  = Ok
  | RoutedTo Int
  | NoRoute
  | Refused Text
  | Deliver Delivery
  deriving (Show, Eq)
-- ANCHOR_END: request

-- ANCHOR: rule
-- 🚧 The rule of the wire: a name or a key is one word, with no space
--    and no newline in it (it may be empty, like the key of a letter to
--    a fanout desk). A body may hold anything, and the reason of
--    REFUSED anything but a newline. Only values that keep the rule
--    read back as they were written.
-- 手順書きの決まり：名前と札は1語。中に空白も改行も入れない（空でもよい。
-- fanout の仕分け台への手紙の札のように）。本文には何を入れてもよく、
-- REFUSED の理由には、改行以外なら何を入れてもよい。書いて読み戻すと
-- 元に戻るのは、決まりを守った値だけ
oneWord :: Text -> Bool
oneWord = T.all (\c -> c /= ' ' && c /= '\n')

-- 🔍 The names and keys in a request that break the rule.
-- 頼みごとの中の、決まりを破っている名前と札
brokenNames :: Request -> [Text]
brokenNames request = filter (not . oneWord) $ case request of
  DeclareQueue q how    -> coerce q : deadDesk how
  DeclareExchange e _   -> [coerce e]
  Bind e key q          -> [coerce e, coerce key, coerce q]
  Publish e m           -> [coerce e, coerce (routingKey m)]
  Consume q _           -> [coerce q]
  Settle _ _            -> []
  where
    deadDesk how = [coerce e | Just e <- [deadLetterTo how]]
-- ANCHOR_END: rule

-- ANCHOR: request-wire
-- ✍️ Writing and reading mirror each other, line by line:
--    "BIND quests quest.dragon.* dragon-team" and so on.
-- 書き方と読み方は、1行ずつ鏡のように対応する：
-- 「BIND quests quest.dragon.* dragon-team」など
instance Wire Request where
  render request = case request of
    DeclareQueue q how    -> line "QUEUE"    (nameB q : settingsB how)
    DeclareExchange e how -> line "EXCHANGE" [nameB e, kindB how]
    Bind e key q          -> line "BIND"     [nameB e, nameB key, nameB q]
    Publish e m           -> line "PUBLISH"  (nameB e : letterB m)
    Consume q n           -> line "CONSUME"  [nameB q, intDec n]
    Settle how (Tag n)    -> line (settleWord how) [intDec n]

  parser = choice $
    [ command "QUEUE"    $ DeclareQueue    <$> field <*> settingsP
    , command "EXCHANGE" $ DeclareExchange <$> field <*> kindP
    , command "BIND"     $ Bind            <$> field <*> field <*> field
    , command "PUBLISH"  $ Publish         <$> field <*> letterP
    , command "CONSUME"  $ Consume         <$> field <*> number
    ] ++
    [ command (settleWord how) $ Settle how . Tag <$> number
    | how <- [minBound .. maxBound] ]
-- ANCHOR_END: request-wire

-- ANCHOR: response-wire
instance Wire Response where
  render response = case response of
    Ok          -> line "OK"      []
    RoutedTo n  -> line "ROUTED"  [intDec n]
    NoRoute     -> line "NOROUTE" []
    Refused why -> line "REFUSED" [textB why]
    Deliver d   -> line "DELIVER" (deliveryB d)

  parser = choice
    [ command "OK"      $ pure Ok
    , command "ROUTED"  $ RoutedTo <$> (char ' ' *> decimal)
    , command "NOROUTE" $ pure NoRoute
    , command "REFUSED" $ Refused  <$> (char ' ' *> restOfLine)
    , command "DELIVER" $ Deliver  <$> deliveryP
    ]
-- ANCHOR_END: response-wire

-- ANCHOR: pieces
-- 🧱 A line: a command word, its pieces after spaces, a newline.
-- 1行：命令の単語と、空白のあとに部品、最後に改行
line :: ByteString -> [Builder] -> Builder
line verb pieces =
  mconcat (intersperse " " (byteString verb : pieces)) <> "\n"

command :: ByteString -> Parser a -> Parser a
command verb pieces = string verb *> pieces <* char '\n'

-- 🏷️ A name after a space. Every name is text inside, so `coerce`
--    turns a word into any of them (and back) at no cost.
-- 空白のあとの名前。どの名前も中身は文字列なので、coerce で、
-- 単語をどの名前にも（その逆にも）、手間なしで変えられる
field :: Coercible Text name => Parser name
field = char ' ' *> (coerce <$> word)

nameB :: Coercible name Text => name -> Builder
nameB = textB . coerce

-- ✉️ A letter: its key, then its body with the length in front.
-- 手紙：札と、長さを先に書いた本文
letterP :: Parser Message
letterP = Message <$> field <*> (char ' ' *> sized)

letterB :: Message -> [Builder]
letterB (Message key text) = [nameB key, sizedB text]

kindP :: Parser ExchangeKind
kindP = char ' ' *> choice [ Fanout <$ string "fanout"
                           , Direct <$ string "direct"
                           , Topic  <$ string "topic" ]

kindB :: ExchangeKind -> Builder
kindB Fanout = "fanout"
kindB Direct = "direct"
kindB Topic  = "topic"
-- 🔢 A number after a space.
-- 空白のあとの数
number :: Parser Int
number = char ' ' *> decimal

-- ✅ "ACK 7", "REQUEUE 7", "REJECT 7".
settleWord :: Settlement -> ByteString
settleWord Ack     = "ACK"
settleWord Requeue = "REQUEUE"
settleWord Reject  = "REJECT"

-- ANCHOR: settingsP
-- ⚙️ Settings are optional words:
--    "QUEUE jobs dead=undeliverable limit=3", "QUEUE scratch transient".
-- 設定は、なくてもよい単語：
-- 「QUEUE jobs dead=undeliverable limit=3」「QUEUE scratch transient」
settingsP :: Parser QueueSettings
settingsP =
  QueueSettings <$> optional (string " dead="  *> (coerce <$> word))
                <*> optional (string " limit=" *> decimal)
                <*> option Durable (Transient <$ string " transient")

settingsB :: QueueSettings -> [Builder]
settingsB (QueueSettings dead limit lasting) =
  [ "dead="  <> nameB e  | Just e <- [dead]  ] ++
  [ "limit=" <> intDec n | Just n <- [limit] ] ++
  [ "transient"          | lasting == Transient ]
-- ANCHOR_END: settingsP

-- 📨 "DELIVER 7 again quest.dragon 5\nhello": the tag, whether it is
--    a second try, and the letter.
-- 「DELIVER 7 again quest.dragon 5\nhello」：番号札、二度目以降か、手紙
deliveryP :: Parser Delivery
deliveryP = do
  n     <- number
  again <- char ' ' *> choice [False <$ string "new", True <$ string "again"]
  m     <- letterP
  pure (Delivery (Tag n) m again)

deliveryB :: Delivery -> [Builder]
deliveryB (Delivery (Tag n) m again) =
  intDec n : (if again then "again" else "new") : letterB m
-- ANCHOR_END: pieces
-- ANCHOR_END: all
