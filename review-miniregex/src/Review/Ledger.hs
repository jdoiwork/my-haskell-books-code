-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- | The old ledger of the records room: fix the dates and the gold, then
--   read each line and add up the sales.
-- 記録室の古い帳簿：日付と金貨の書き方を直してから、1行ずつ読んで、
-- 売り上げを足す
module Review.Ledger
  ( -- * Step 1 / ステップ1
    oldPage
  , oldDate
  , fixDates
    -- * Step 2 / ステップ2
  , draftGold
  , fixLineDraft
  , Entry (..)
  , entryRegex
  , readEntry
  , totalsWith
  , totals
    -- * Step 3 / ステップ3
  , goldRegex
  , fixLine
    -- * Step 4 / ステップ4
  , asciiDate
  , asciiGold
  , fixLineAscii
  , datePlan
  , goldPlan
  ) where

-- Text, and the table of the totals
-- 文字列と、合計の表
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

-- MiniRegex: the easy face, the quasi-quoters and the typed groups
-- MiniRegex：使いやすい顔、準クォート、型付きのグループ
import MiniRegex
import MiniRegex.Quote (re, typed)
import MiniRegex.Quoted (readInt)
import MiniRegex.Typed (Part (..), TypedRegex)
import MiniRegex.Typed qualified as Typed
-- ANCHOR_END: head

-- ANCHOR: page
-- 📜 A page of the old ledger.
-- 古い帳簿の1ページ
oldPage :: [Text]
oldPage =
  [ "21/11/2026 Mira sold 300g for herbs"
  , "2026-11-21 Ruby sold 25 G"
  , "22/11/2026 Mira sold 120 gold"
  , "22/11/2026 the cat slept"
  ]
-- ANCHOR_END: page

-- ANCHOR: dates
-- 📅 An old date: 21/11/2026 (day, month, year).
-- 古い日付：21/11/2026（日・月・年）
oldDate :: Regex
oldDate = [re|(?<day>\d{2})/(?<month>\d{2})/(?<year>\d{4})|]

-- 📅 Write every old date the new way: 2026-11-21.
-- 古い日付を、全部新しい書き方に：2026-11-21
fixDates :: Text -> Text
fixDates = replaceAllTemplate oldDate (templateOf oldDate datePlan)

-- 📝 The new way, as a template.
-- 新しい書き方を、テンプレートで
datePlan :: Text
datePlan = "${year}-${month}-${day}"

-- 🧩 A template we wrote ourselves. A wrong one (a name that the
--    pattern does not have) stops the program the first time it is used.
-- 自分で書いたテンプレート。まちがい（パターンにない名前など）は、
-- 初めて使ったときにプログラムを止める
templateOf :: Regex -> Text -> Template
templateOf r plan = case template r plan of
  Right t  -> t
  Left err -> error (T.unpack (explain plan err))
-- ANCHOR_END: dates

-- ANCHOR: draft
-- 💰 The first try: 300g, 300 G and 300 gold all become 300 gold.
-- 最初の下書き：300g も 300 G も 300 gold も、300 gold に
draftGold :: Regex
draftGold = [re|(?i)(\d+) ?g|]

-- 🧹 Fix a line with the first try.
-- 最初の下書きで、1行を直す
fixLineDraft :: Text -> Text
fixLineDraft =
  replaceAllTemplate draftGold (templateOf draftGold goldPlan) . fixDates

-- 📝 What the gold becomes.
-- 金貨を何に書き換えるか
goldPlan :: Text
goldPlan = "$1 gold"
-- ANCHOR_END: draft

-- ANCHOR: entry
-- 🧾 A line of the ledger, read.
-- 読んだ帳簿の1行
data Entry = Entry
  { date :: Text
  , who  :: Text
  , gold :: Int
  , item :: Maybe Text
  }
  deriving (Show, Eq)

-- 🏷️ A fixed line: 2026-11-21 Mira sold 300 gold for herbs.
--    The date, who and gold are always there; the item is not.
--    With (?x), spaces and new lines in the pattern are not read, so
--    a space to find is written "\ ".
-- 直した1行：2026-11-21 Mira sold 300 gold for herbs。日付・人・金貨は
-- いつもあり、品物はないこともある。(?x) を付けると、パターンの中の
-- 空白と改行は読まれないので、探す空白は「\ 」と書く
entryRegex
  :: TypedRegex '[ '("date", 'Always), '("who", 'Always)
                 , '("gold", 'Always), '("item", 'Sometimes) ]
entryRegex = [typed|(?x)
  ^ (?<date> \d{4}-\d{2}-\d{2} )
  \  (?<who> \w+ ) \ sold
  \  (?<gold> \d+ ) \ gold
  (?: \ for \ (?<item> \w+ ) )? $
  |]

-- 📖 Read a fixed line. m.date is a Text; m.item is a Maybe Text.
-- 直した1行を読む。m.date は Text、m.item は Maybe Text
readEntry :: Text -> Maybe Entry
readEntry line = do
  m <- Typed.find entryRegex line
  g <- readInt m.gold
  pure (Entry m.date m.who g m.item)
-- ANCHOR_END: entry

-- ANCHOR: totals
-- ➕ Fix every line with fix, read it, and add up the gold of each
--    person. A line that cannot be read is left out.
-- どの行も fix で直して読み、人ごとに金貨を足す。読めない行は数えない
totalsWith :: (Text -> Text) -> [Text] -> Map Text Int
totalsWith fix ls =
  Map.fromListWith (+)
    [ (e.who, e.gold) | Just e <- map (readEntry . fix) ls ]
-- ANCHOR_END: totals

-- ANCHOR: fixed
-- 💰 The gold again: the g must end a word, and "gold" may be written
--    out already. 300 gold stays 300 gold.
-- 金貨を、もう一度。g で単語が終わること、「gold」と書き切ってあっても
-- よいこと。300 gold は 300 gold のまま
goldRegex :: Regex
goldRegex = [re|(?i)(\d+) ?g(?:old)?\b|]

-- 🧹 Fix a line: the dates, then the gold.
-- 1行を直す：日付、それから金貨
fixLine :: Text -> Text
fixLine =
  replaceAllTemplate goldRegex (templateOf goldRegex goldPlan) . fixDates

-- ➕ The totals, with the fixed fix.
-- 直した版で、合計を
totals :: [Text] -> Map Text Int
totals = totalsWith fixLine
-- ANCHOR_END: fixed

-- ANCHOR: ascii
-- 🔤 The same two patterns, with (?a): \d, \w and \b are ASCII only.
--    The ledger writes its numbers in ASCII.
-- 同じ2つのパターンに (?a)：\d・\w・\b は ASCII だけ。帳簿の数は
-- ASCII で書く決まり
asciiDate :: Regex
asciiDate = [re|(?a)(?<day>\d{2})/(?<month>\d{2})/(?<year>\d{4})|]

asciiGold :: Regex
asciiGold = [re|(?a)(?i)(\d+) ?g(?:old)?\b|]

-- 🧹 Fix a line, the ASCII way.
-- ASCII の決まりで、1行を直す
fixLineAscii :: Text -> Text
fixLineAscii =
  replaceAllTemplate asciiGold (templateOf asciiGold goldPlan)
    . replaceAllTemplate asciiDate (templateOf asciiDate datePlan)
-- ANCHOR_END: ascii
-- ANCHOR_END: all
