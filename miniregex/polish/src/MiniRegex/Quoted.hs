-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- | The jobs of the records room, written with [re|…|] and [typed|…|].
-- 記録室の仕事を、[re|…|] と [typed|…|] で書いたもの
module MiniRegex.Quoted
  ( dateRegex
  , datesOf
  , saleRegex
  , salesOf
  , Sale (..)
  , datesTyped
  , saleTyped
  , salesOfTyped
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

import MiniRegex
import MiniRegex.Quote (re, typed)
import MiniRegex.Typed (Part (..), TypedRegex)
import MiniRegex.Typed qualified as Typed

-- ANCHOR_END: imports

-- ANCHOR: dates
-- 📅 A date: 2026-11-21. \d is written as it is, with no "\\d".
-- 日付：2026-11-21。\d は "\\d" にせず、そのまま書ける
dateRegex :: Regex
dateRegex = [re|\d{4}-\d{2}-\d{2}|]

-- 📅 All the dates in a line.
-- 1行の中の日付を全部
datesOf :: Text -> [Text]
datesOf line = map matchText (findAll dateRegex line)
-- ANCHOR_END: dates

-- ANCHOR: sales
-- 💰 "Mira sold 300 gold", maybe with " for herbs".
-- 「Mira sold 300 gold」。うしろに「 for herbs」が付くこともある
saleRegex :: Regex
saleRegex =
  [re|(?<who>\w+) sold (?<gold>\d+) gold(?: for (?<item>\w+))?|]

-- 🧾 A sale, the way we use it.
-- 使いやすい形にした売り上げ
data Sale = Sale
  { who  :: Text
  , gold :: Int
  , item :: Maybe Text
  }
  deriving (Show, Eq)

-- 🧾 The sales in a line, with named: each group is a Maybe.
-- 1行の中の売り上げ。named を使うと、どのグループも Maybe
salesOf :: Text -> [Sale]
salesOf line =
  [ Sale w g (named "item" m)
  | m <- findAll saleRegex line
  , Just w <- [named "who" m]
  , Just g <- [readMaybe . T.unpack =<< named "gold" m] ]
-- ANCHOR_END: sales

-- ANCHOR: typed
-- 🏷️ The same date, with its parts in the type.
-- 同じ日付を、部分を型に書いて
datesTyped
  :: TypedRegex '[ '("year", 'Always), '("month", 'Always)
                 , '("day", 'Sometimes) ]
datesTyped = [typed|(?<year>\d{4})-(?<month>\d{2})(?:-(?<day>\d{2}))?|]
-- ANCHOR_END: typed

-- ANCHOR: salestyped
-- 💰 The sale again, typed: who and gold are always set, item is not.
-- 売り上げを、もう一度、型付きで。who と gold は必ず使われ、item は
-- 使われないこともある
saleTyped
  :: TypedRegex '[ '("who", 'Always), '("gold", 'Always)
                 , '("item", 'Sometimes) ]
saleTyped =
  [typed|(?<who>\w+) sold (?<gold>\d+) gold(?: for (?<item>\w+))?|]

-- 🧾 The sales in a line: m.who is a Text, m.item a Maybe Text.
-- 1行の中の売り上げ。m.who は Text、m.item は Maybe Text
salesOfTyped :: Text -> [Sale]
salesOfTyped line =
  [ Sale m.who g m.item
  | m <- Typed.findAll saleTyped line
  , Just g <- [readMaybe (T.unpack m.gold)] ]
-- ANCHOR_END: salestyped
-- ANCHOR_END: all
