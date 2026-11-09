-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE OverloadedRecordDot #-}
-- | What a review site does with the cards, all pure: spoilers hidden by
--   how far I have read, the statistics of reading, works that look
--   alike by their tags, and writing the shelf out as Markdown and CSV.
--   The window is not touched; every function takes the cards it is
--   given (listCards is enough).
-- レビューサイトがカードですること。全部が純粋：どこまで読んだかでネタバレを
-- 隠す、読書の統計、タグで似ている作品、本棚を Markdown と CSV に書き出す。
-- 窓口にはさわらない。どの関数も、渡されたカードだけを使う（listCards で十分）
module Tsundoku.Review
  ( -- * Spoilers / ネタバレ
    Seen (..)
  , seenNotes
  , hiddenHint
    -- * Statistics / 統計
  , Month (..)
  , MonthStat (..)
  , finishedOn
  , monthly
  , tagCloud
  , kindShare
  , yearTop
    -- * Works alike / 似ている作品
  , jaccard
  , similarTo
    -- * Writing out / 書き出し
  , toMarkdown
  , toCsv
  , csvField
  ) where

-- lists, maps, sets, text, and dates
-- リスト、辞書、集合、文字列、日付
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, toGregorian)
import Data.Time.Format.ISO8601 (iso8601Show)

import Tsundoku.Card (Card (..))
import Tsundoku.Labels (stateLabel)
import Tsundoku.Progress (finishedIn, perDay)
import Tsundoku.Rating (RatingStyle (..), render, scoreValue)
import Tsundoku.Search (trailTo)
import Tsundoku.Shelf.Rows (toPlain)
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: spoilers
-- 🙈 A note as I may see it now. A hidden note keeps only where it was
--    written, not its body: a screen cannot show what it was not given.
-- いまの自分に見せてよい感想。隠した感想は、どこに書いたかだけを持ち、
-- 本文は持たない。渡されていないものは、画面も見せようがない
data Seen = Shown Note | Hidden Target
  deriving (Show, Eq)

-- 🙈 The notes of a card, oldest first. A spoiler on an entry shows once
--    that entry is read; a spoiler on the whole work, once the work is
--    finished. Notes that are not spoilers always show.
-- カードの感想を古い順に。項目へのネタバレは、その項目を読んだら見える。
-- 作品全体へのネタバレは、読み終えたら見える。ネタバレでない感想は、
-- いつも見える
seenNotes :: Card -> [Seen]
seenNotes card = map see card.notes
  where
    over     = card.reading `elem` [Finished, Rereading]
    done     = finishedIn card.readLog
    see note = case note.target of
      _ | not note.spoiler || over     -> Shown note
      OnEntry e | e `Set.member` done  -> Shown note
      target                           -> Hidden target

-- 💬 Why a note is hidden, and when it will show.
-- 感想を隠している理由と、いつ見えるようになるか
hiddenHint :: Contents -> Target -> Text
hiddenHint contents target = case target of
  OnWork    -> "ネタバレ（読み終えたら見えます）"
  OnEntry e -> case reverse (trailTo e contents) of
    heading : _ -> "ネタバレ（「" <> heading <> "」を読んだら見えます）"
    []          -> "ネタバレ"
-- ANCHOR_END: spoilers

-- ANCHOR: stats
-- 📅 A month of a year.
-- ある年の、ある月
data Month = Month { year :: Integer, month :: Int }
  deriving (Show, Eq, Ord)

-- 📊 What a month holds: works finished, and pages read. Two months'
--    worth add up, so a Map can gather them with (<>).
-- 1か月ぶん：読み終えた作品の数と、読んだページ数。足し合わせられるので、
-- 辞書は (<>) で集められる
data MonthStat = MonthStat { finished :: Int, pagesRead :: Pages }
  deriving (Show, Eq)

instance Semigroup MonthStat where
  MonthStat a p <> MonthStat b q = MonthStat (a + b) (p <> q)

instance Monoid MonthStat where
  mempty = MonthStat 0 mempty

-- 🏁 The day a work was finished: the last day in its log, if it is
--    finished (or being read again).
-- 作品を読み終えた日：読み終えている（か、読み返している）なら、ログの
-- 最後の日
finishedOn :: Card -> Maybe Day
finishedOn card
  | card.reading `elem` [Finished, Rereading], not (null days) = Just (maximum days)
  | otherwise                                                    = Nothing
  where
    days = map readOn card.readLog

-- 📅 Month by month: works finished, and pages read (books only; other
--    works are not counted in pages).
-- 月ごと：読み終えた作品と、読んだページ（本だけ。ほかの作品はページで
-- 数えない）
monthly :: [Card] -> Map Month MonthStat
monthly cards = Map.unionsWith (<>) (map one cards)
  where
    one card = Map.fromListWith (<>) (ends card ++ pages card)
    ends card  = [ (monthOf d, MonthStat 1 mempty) | Just d <- [finishedOn card] ]
    pages card = case card.work.contents of
      Book toc -> [ (monthOf d, MonthStat 0 p)
                  | (d, p) <- Map.toList (perDay toc card.readLog) ]
      _        -> []
    monthOf d = let (y, m, _) = toGregorian d in Month y m

-- ☁️ How many works wear each tag: most first, then by name.
-- タグごとに、何作品に付いているか：多い順、同じなら名前の順
tagCloud :: [Card] -> [(Text, Int)]
tagCloud cards = sortOn (\(t, n) -> (Down n, t))
  (Map.toList (Map.fromListWith (+) [ (t, 1) | c <- cards, t <- Set.toList c.tags ]))

-- 🍰 How many works of each kind ("book", "serial", ...).
-- 種類（"book"・"serial" など）ごとに、何作品あるか
kindShare :: [Card] -> [(Text, Int)]
kindShare cards = Map.toList (Map.fromListWith (+) [ (kindOf c, 1) | c <- cards ])
  where
    kindOf c = let (word, _, _) = toPlain c.work.contents in word

-- 🏆 The best n works finished in a year, by their rating (the unrated
--    last). The shelf's order breaks ties.
-- その年に読み終えた作品のうち、評価の高い n 冊（評価のないものは最後）。
-- 同じなら本棚の順
yearTop :: Integer -> Int -> [(WorkId, Card)] -> [(WorkId, Card)]
yearTop y n cards = take n (sortOn (Down . points . snd) thatYear)
  where
    thatYear = [ wc | wc@(_, c) <- cards, Just d <- [finishedOn c], yearOf d == y ]
    yearOf d = let (yy, _, _) = toGregorian d in yy
    points c = maybe (-1) scoreValue c.rating
-- ANCHOR_END: stats

-- ANCHOR: similar
-- 🤝 How alike two sets are: what they share, out of what either has
--    (the Jaccard index). Two empty sets share nothing to speak of: 0.
-- 2つの集合がどれだけ似ているか：どちらかにあるもののうち、両方にあるものの
-- 割合（Jaccard 係数）。どちらも空なら、語れる共通点はないので 0
jaccard :: Ord a => Set a -> Set a -> Rational
jaccard a b
  | Set.null both = 0
  | otherwise     = fromIntegral (Set.size shared) / fromIntegral (Set.size both)
  where
    shared = Set.intersection a b
    both   = Set.union a b

-- 🤝 The n works most like this one by their tags, most alike first.
--    Works that share no tag are left out.
-- タグがこの作品に似ている n 作品。似ている順。タグを1つも共有しない作品は
-- 外す
similarTo :: Int -> WorkId -> [(WorkId, Card)] -> [(Rational, WorkId, Card)]
similarTo n wid cards = case lookup wid cards of
  Nothing   -> []
  Just mine -> take n (sortOn (\(r, _, _) -> Down r)
    [ (r, other, c) | (other, c) <- cards, other /= wid
                    , let r = jaccard mine.tags c.tags, r > 0 ])
-- ANCHOR_END: similar

-- ANCHOR: export
-- 📝 The shelf as Markdown: a section for each work, with what I may
--    see now (hidden spoilers stay hidden in the file too).
-- 本棚を Markdown に：作品ごとに節を1つ。いまの自分に見せてよいものだけ
-- （隠したネタバレは、ファイルの中でも隠したまま）
toMarkdown :: [(WorkId, Card)] -> Text
toMarkdown cards = T.unlines (concatMap section cards)
  where
    section (_, c) =
      [ "## " <> c.work.title <> creatorOf c, ""
      , "- " <> T.intercalate " / " (facts c) ]
      ++ [ "" | not (null (seenNotes c)) ]
      ++ map (note c) (seenNotes c) ++ [""]
    creatorOf c = if T.null c.work.creator then "" else "（" <> c.work.creator <> "）"
    facts c = [stateLabel c.reading]
              ++ [ render FiveStars s | Just s <- [c.rating] ]
              ++ [ T.unwords (map ("#" <>) (Set.toList c.tags)) | not (Set.null c.tags) ]
    note _ (Shown n)  = "> " <> n.body
    note c (Hidden t) = "> （" <> hiddenHint c.work.contents t <> "）"

-- 📄 The shelf as CSV (RFC 4180): a header, then one row for each work.
--    Points stay points (0-100), tags are joined with a space.
-- 本棚を CSV（RFC 4180）に：見出しの行、それから作品ごとに1行。
-- 点は点のまま（0〜100）、タグは空白でつなぐ
toCsv :: [(WorkId, Card)] -> Text
toCsv cards = T.concat (map row (header : map fields cards))
  where
    header = ["id", "title", "creator", "kind", "reading", "rating", "tags", "finished_on"]
    fields (WorkId n, c) =
      [ T.pack (show n), c.work.title, c.work.creator, kindOf c
      , T.pack (show c.reading), maybe "" (T.pack . show . scoreValue) c.rating
      , T.unwords (Set.toList c.tags)
      , maybe "" (T.pack . iso8601Show) (finishedOn c) ]
    row xs = T.intercalate "," (map csvField xs) <> "\r\n"
    kindOf c = let (word, _, _) = toPlain c.work.contents in word

-- 🧷 One field of CSV. A field with a comma, a quote, or a line break is
--    put in quotes, and a quote inside is written twice.
-- CSV のフィールド1つ。カンマ・引用符・改行を含むなら引用符でくるみ、
-- 中の引用符は2つ重ねる
csvField :: Text -> Text
csvField t
  | T.any (`elem` [',', '"', '\n', '\r']) t = "\"" <> T.replace "\"" "\"\"" t <> "\""
  | otherwise                                = t
-- ANCHOR_END: export
-- ANCHOR_END: all
