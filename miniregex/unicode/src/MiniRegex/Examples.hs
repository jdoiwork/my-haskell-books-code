-- ANCHOR: all
-- ANCHOR: header
-- | The records of the guild's archive room, a search by hand, and the
--   code we want to write with MiniRegex.
-- ギルドの記録室の記録と、手で書いた探しものと、MiniRegex で書きたい
-- コード
module MiniRegex.Examples
  ( records
  , datesByHand
  , looksLikeDate
  , findDates
  , countDragons
  , renameDragons
  , splitLoot
  , surely
  , see
  , endsOf
  , endsAtStart
  , datePattern
  , dateParts
  , toDayFirst
  , toDayFirstT
  , notes
  , doubled
  , doubledWords
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Data.Text.IO qualified as TIO

import MiniRegex
import MiniRegex.Candidates (ends)
import MiniRegex.Cursor (Cursor (..))
import MiniRegex.Parser (parsePattern)
import MiniRegex.Pretty (drawPattern)
import MiniRegex.Types (tree)
-- ANCHOR_END: imports

-- ANCHOR: records
-- 📜 A page of the old records.
-- 古い記録の1ページ
records :: Text
records = T.unlines
  [ "2026-11-19 Mira sold a dragon scale: 300 gold"
  , "2026-11-20 Ruby bought herb, sword, shield"
  , "On 2026-11-21 a dragon was seen near the lake"
  , "Room 1234-56-789 is closed"
  ]
-- ANCHOR_END: records

-- ANCHOR: byhand
-- ✋ The dates like 2026-11-19, found by hand: look at 10 characters
--    from each place, and keep them if they look like a date.
-- 2026-11-19 のような日付を、手で探す：1文字ずつずらしながら10文字を
-- 取り出し、日付に見えたら残す
datesByHand :: Text -> [Text]
datesByHand t
  | T.length t < 10         = []
  | looksLikeDate candidate = candidate : datesByHand (T.drop 10 t)
  | otherwise               = datesByHand (T.drop 1 t)
  where
    candidate = T.take 10 t

-- 🗓️ Four digits, -, two digits, -, two digits.
-- 数字4つ、-、数字2つ、-、数字2つ
looksLikeDate :: Text -> Bool
looksLikeDate d =
  T.length d == 10
    && T.all isDigit year
    && T.index d 4 == '-'
    && T.all isDigit month
    && T.index d 7 == '-'
    && T.all isDigit day
  where
    year  = T.take 4 d
    month = T.take 2 (T.drop 5 d)
    day   = T.take 2 (T.drop 8 d)
-- ANCHOR_END: byhand

-- ANCHOR: wanted
-- 📅 The dates, the way we want to write it. A wrong pattern stops
--    at compile, before any record is read.
-- 書きたい形で日付を探す。まちがったパターンは compile で止まり、
-- 記録を読む前にわかる
findDates :: Text -> Either RegexError [Text]
findDates t = do
  date <- compile "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  pure (map matchText (findAll date t))

-- 🐉 How many times a dragon is in the records.
-- 記録の中に、ドラゴンが何回出てくるか
countDragons :: Text -> Either RegexError Int
countDragons t = do
  dragon <- compile "dragon"
  pure (length (findAll dragon t))

-- 🦎 The guild calls them wyverns now.
-- ギルドは、いまはワイバーンと呼ぶことにした
renameDragons :: Text -> Either RegexError Text
renameDragons t = do
  dragon <- compile "dragon"
  pure (replaceAll dragon "wyvern" t)

-- 🎒 "herb, sword, shield" into the items.
-- "herb, sword, shield" を、品物ごとに切る
splitLoot :: Text -> Either RegexError [Text]
splitLoot t = do
  comma <- compile ", "
  pure (split comma t)
-- ANCHOR_END: wanted

-- ANCHOR: surely
-- 🧪 For GHCi: a pattern we know is fine.
-- GHCi で試す用：正しいとわかっているパターン
surely :: Text -> Regex
surely src = either (error . show) id (compile src)
-- ANCHOR_END: surely

-- ANCHOR: see
-- 👀 For GHCi: read a pattern, and draw its tree, or point at the
--    trouble. It prints, so it is an IO.
-- GHCi で試す用：パターンを読んで、木を描く。まちがいなら、その場所を
-- 指す。画面に書くので IO
see :: Text -> IO ()
see src = case parsePattern [] src of
  Right tree -> TIO.putStr (drawPattern tree)
  Left err   -> TIO.putStr (explain src err)
-- ANCHOR_END: see

-- ANCHOR: ends
-- 🧺 For GHCi: the places (character positions) that the list matcher
--    lists, in order, when the pattern starts at the head of the text.
-- GHCi で試す用：パターンが文字列の頭から始まるとき、候補のリストの
-- マッチャーが並べる場所（何文字目か）を、順に
endsOf :: Text -> Text -> [Int]
endsOf p t = map (at . fst) (ends [] (tree (surely p)) (Cursor 0 Nothing t))

-- 🧮 How many of them. The pieces after the pattern are tried once at
--    each of them.
-- その数。パターンのうしろの部品は、その1つずつで試される
endsAtStart :: Text -> Text -> Int
endsAtStart p t = length (endsOf p t)
-- ANCHOR_END: ends

-- ANCHOR: dates
-- 📅 A date, with a name for each part.
-- 日付。部分ごとに名前を付けた
datePattern :: Regex
datePattern =
  surely "(?<year>[0-9]{4})-(?<month>[0-9]{2})-(?<day>[0-9]{2})"

-- 🗓️ Each date in the text, as (year, month, day).
-- 文字列の中の日付を、(年, 月, 日) の組で
dateParts :: Text -> [(Text, Text, Text)]
dateParts t =
  [ (part "year" m, part "month" m, part "day" m)
  | m <- findAll datePattern t ]

-- 🧩 A named group, or "" (the three of datePattern always take part).
-- 名前付きのグループ。なければ ""（datePattern の3つは、いつも使われる）
part :: Text -> Match -> Text
part name m = fromMaybe "" (named name m)
-- ANCHOR_END: dates

-- ANCHOR: dayfirst
-- 🔁 2026-11-19 into 19/11/2026, with a function.
-- 2026-11-19 を 19/11/2026 に。関数で
toDayFirst :: Text -> Text
toDayFirst = replaceWith datePattern dayFirst
  where
    dayFirst m = part "day" m <> "/" <> part "month" m <> "/" <> part "year" m
-- ANCHOR_END: dayfirst

-- ANCHOR: dayfirstT
-- 🧾 The same, with a template. A wrong template stops here, before
--    any record is read.
-- 同じことを、テンプレートで。まちがったテンプレートは、記録を読む前に、
-- ここで止まる
toDayFirstT :: Text -> Either RegexError Text
toDayFirstT t = do
  tpl <- template datePattern "${day}/${month}/${year}"
  pure (replaceAllTemplate datePattern tpl t)
-- ANCHOR_END: dayfirstT

-- ANCHOR: doubled
-- 📝 A clerk's note, with some words written twice.
-- 係の人のメモ。2回書いてしまった言葉がある
notes :: Text
notes = "Mira saw the the dragon near near the lake"

-- 🦜 A word, a space, and the same word again.
-- 言葉、空白、そして同じ言葉をもう一度
doubled :: Regex
doubled = surely "([a-z]+) \\1"

-- 🦜 The words written twice.
-- 2回書いてしまった言葉
doubledWords :: Text -> [Text]
doubledWords t = [w | m <- findAll doubled t, Just w <- [group 1 m]]
-- ANCHOR_END: doubled
-- ANCHOR_END: all
