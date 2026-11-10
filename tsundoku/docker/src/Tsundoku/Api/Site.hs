-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE TemplateHaskell       #-}
-- | The review site on the wire: tags, notes as I may see them now,
--   works alike, and the statistics. The core (Tsundoku.Review) already
--   knows all of them; this only puts them into plain shapes for the
--   screens written outside Haskell.
-- 線の上のレビューサイト：タグ、いまの自分に見せてよい感想、似ている作品、
-- 統計。どれも核（Tsundoku.Review）がもう知っている。ここは、Haskell の
-- 外で書く画面のために、素朴な形にするだけ
module Tsundoku.Api.Site
  ( ExtrasItem (..)
  , SeenNote (..)
  , AlikeItem (..)
  , TagCount (..)
  , KindCount (..)
  , MonthItem (..)
  , StatsItem (..)
  , extrasItem
  , tagCounts
  , taggedWith
  , statsItem
  , percentOf
  ) where

-- maps, sets, text, and dates
-- 辞書、集合、文字列、日付
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Printf (printf)

-- JSON, and TypeScript types made from the same types
-- JSON と、同じ型から作る TypeScript の型
import Data.Aeson (FromJSON, ToJSON, defaultOptions)
import Data.Aeson.TypeScript.TH (deriveTypeScript)

import Tsundoku.Api.Json (NoteItem, WorkSummary, noteItem, summaryOf)
import Tsundoku.Card (Card (..))
import Tsundoku.Review
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: extras
-- 🔖 The extras of one work: its tags, its notes as I may see them now,
--    and the works most like it.
-- 作品1つのおまけ：タグ、いまの自分に見せてよい感想、似ている作品
data ExtrasItem = ExtrasItem
  { tags  :: [Text]
  , seen  :: [SeenNote]
  , alike :: [AlikeItem]
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 🙈 A note, shown or hidden. A hidden one carries where it was written
--    and the hint, never its body: the core's Seen, onto the wire.
-- 感想。見せるか、隠すか。隠した感想は、どこに書いたかとヒントだけを運び、
-- 本文は運ばない。核の Seen を、線の上へ
data SeenNote
  = ShownNote  { note :: NoteItem }
  | HiddenNote { entry :: Maybe Int, hint :: Text }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 🤝 A work alike, and how alike, in percent (the core's rounding).
-- 似ている作品と、どれだけ似ているか。百分率で（丸めは核で）
data AlikeItem = AlikeItem
  { work    :: Int
  , title   :: Text
  , percent :: Int
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
-- ANCHOR_END: extras

-- ANCHOR: stats
-- ☁️ A tag, and how many works wear it.
-- タグと、それを付けた作品の数
data TagCount = TagCount
  { tag   :: Text
  , works :: Int
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 🍰 A kind ("book", "serial", ...), and how many works of it.
-- 種類（"book"・"serial" など）と、その作品の数
data KindCount = KindCount
  { kind  :: Text
  , works :: Int
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 📅 A month: "2026-11", works finished, and pages read.
-- 1か月："2026-11"、読み終えた作品の数、読んだページ数
data MonthItem = MonthItem
  { month    :: Text
  , finished :: Int
  , pages    :: Int
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 📊 The statistics of a year's page: the months (oldest first), the
--    kinds, and the best works finished in that year.
-- 統計のページ1年ぶん：月ごと（古い順）、種類ごと、その年のベスト
data StatsItem = StatsItem
  { year   :: Int
  , months :: [MonthItem]
  , kinds  :: [KindCount]
  , best   :: [WorkSummary]
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
-- ANCHOR_END: stats

-- ANCHOR: extrasItem
-- 🔖 The extras of a work, from all the cards (to find works alike).
--    A hidden note becomes its hint here, so its body never leaves.
-- 作品のおまけを、全部のカードから作る（似ている作品を探すため）。
-- 隠した感想は、ここでヒントになる。本文は、ここから先へ出ていかない
extrasItem :: [(WorkId, Card)] -> WorkId -> Card -> ExtrasItem
extrasItem cards wid card = ExtrasItem
  { tags  = Set.toList card.tags
  , seen  = map onWire (seenNotes card)
  , alike = [ AlikeItem n c.work.title (percentOf r)
            | (r, WorkId n, c) <- similarTo 3 wid cards ]
  }
  where
    onWire s = case s of
      Shown n  -> ShownNote (noteItem n)
      Hidden t -> HiddenNote (entryOf t) (hiddenHint card.work.contents t)
    entryOf t = case t of
      OnWork              -> Nothing
      OnEntry (EntryId e) -> Just e

-- 💯 A share (0 to 1) in percent; the middle rounds up (1/8 → 13).
-- 割合（0〜1）を百分率に。真ん中は切り上げ（1/8 → 13）
percentOf :: Rational -> Int
percentOf r = floor (r * 100 + 1 / 2)
-- ANCHOR_END: extrasItem

-- ANCHOR: statsItem
-- ☁️ The tag cloud, and the works that wear one tag.
-- タグの雲と、あるタグを付けた作品
tagCounts :: [(WorkId, Card)] -> [TagCount]
tagCounts cards = [ TagCount t n | (t, n) <- tagCloud (map snd cards) ]

taggedWith :: Text -> [(WorkId, Card)] -> [WorkSummary]
taggedWith t cards =
  [ summaryOf wc | wc@(_, c) <- cards, t `Set.member` c.tags ]

-- 📊 The statistics of a year: its months only, the kinds of the whole
--    shelf, and its best ten.
-- 1年の統計：その年の月だけ、本棚の全体の種類、その年のベスト10
statsItem :: Integer -> [(WorkId, Card)] -> StatsItem
statsItem y cards = StatsItem
  { year   = fromInteger y
  , months = [ MonthItem (monthWord m) s.finished (count s.pagesRead)
             | (m, s) <- Map.toList (monthly (map snd cards)), m.year == y ]
  , kinds  = [ KindCount k n | (k, n) <- kindShare (map snd cards) ]
  , best   = map summaryOf (yearTop y 10 cards)
  }
  where
    monthWord m = T.pack (printf "%d-%02d" m.year m.month)
-- ANCHOR_END: statsItem

-- ANCHOR: typescript
-- 🟦 TypeScript types for the shapes above (aeson's defaults, as in
--    Tsundoku.Api.Json). SeenNote has two constructors, so its JSON
--    carries a "tag" saying which one, and TypeScript gets a union.
-- 上の形の TypeScript の型（Tsundoku.Api.Json と同じく aeson の既定）。
-- SeenNote はコンストラクターが2つなので、JSON はどちらかを言う "tag" を
-- 持ち、TypeScript には合併型ができる
$(concat <$> traverse (deriveTypeScript defaultOptions)
    [ ''ExtrasItem, ''SeenNote, ''AlikeItem
    , ''TagCount, ''KindCount, ''MonthItem, ''StatsItem
    ])
-- ANCHOR_END: typescript
-- ANCHOR_END: all
