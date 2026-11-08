-- ANCHOR: all
-- | A reading card: one work and everything I keep about it.
--   The pure core: functions that take a card and give a new one.
-- 読書カード：作品1つと、それについて書きためたもの全部。
-- 純粋な核：カードを受け取って、新しいカードを返す関数たち
-- ANCHOR: header
module Tsundoku.Card
  ( Card (..)
  , newCard
  , addNote
  , applyEvent
  , rate
  , visibleTo
  , logReading
  , rateEntry
  , ratingsOf
  ) where

-- `when`, dictionaries, Maybe, and text
-- when、辞書、Maybe、文字列
import Control.Monad (when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Text qualified as T

import Tsundoku.Rating (Score)
import Tsundoku.Reading (advance)
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: card
-- 🗂️ One card of the library: the work, where I am, my rating of
--    the whole work, my notes, my reading log (both oldest first),
--    and my ratings of single entries, like episodes.
-- 図書室の読書カード1枚：作品、いまの状態、作品全体の評価、感想、
-- 読書ログ（感想とログは古い順）、目次の項目（話など）ごとの評価
data Card = Card
  { work         :: Work
  , reading      :: ReadingState
  , rating       :: Maybe Score
  , notes        :: [Note]
  , readLog      :: [LogLine]
  , entryRatings :: Map EntryId Score
  }
  deriving (Show, Eq)

-- 🆕 A new card starts on the pile, with no rating, no notes,
--    and nothing in the log.
-- 新しいカードは、積読から。評価も感想も、ログもまだない
newCard :: Work -> Card
newCard w = Card
  { work         = w
  , reading      = Piled
  , rating       = Nothing
  , notes        = []
  , readLog      = []
  , entryRatings = Map.empty
  }
-- ANCHOR_END: card

-- ANCHOR: addNote
-- ✍️ Add a note, if it says something and is about something real.
-- 感想を足す。中身があって、ちゃんとある所に付くときだけ
addNote :: Note -> Card -> Either Mistake Card
addNote note card = do
  when (T.null (T.strip (body note))) (Left EmptyNote)
  checkTarget (contents (work card)) (target note)
  pure card { notes = notes card ++ [note] }

-- 🎯 The whole work is always there; an entry must be in the table.
-- 作品全体は、いつでもある。目次の項目は、目次になければならない
checkTarget :: Contents -> Target -> Either Mistake ()
checkTarget _    OnWork = Right ()
checkTarget what (OnEntry i)
  | i `elem` entryIds what = Right ()
  | otherwise              = Left (NoSuchEntry i)
-- ANCHOR_END: addNote

-- ANCHOR: applyEvent
-- 🔔 Something happened: move the card on the map of reading.
-- できごとが起きた：カードを、読書の地図の上で進める
applyEvent :: Event -> Card -> Either Mistake Card
applyEvent event card = do
  next <- advance event (reading card)
  pure card { reading = next }

-- ⭐ Rate the work (a new rating replaces the old one). Always a
--    score: how it was typed in, in stars or out of ten, is forgotten.
-- 評価を付ける（付け直したら、新しいほうにする）。いつも点で持つ。
-- ★で打ったか、10点満点で打ったかは、覚えない
rate :: Score -> Card -> Card
rate r card = card { rating = Just r }
-- ANCHOR_END: applyEvent

-- ANCHOR: visibleTo
-- 🙈 The notes someone may see: spoilers only after the end.
-- その人に見せてよい感想：ネタバレは、読み終えてから
visibleTo :: ReadingState -> [Note] -> [Note]
visibleTo state allNotes
  | state `elem` [Finished, Rereading] = allNotes
  | otherwise = [ note | note <- allNotes, not (spoiler note) ]
-- ANCHOR_END: visibleTo

-- ANCHOR: logReading
-- 📓 Write a line in the reading log, if the entry is in the table.
-- 読書ログに1行書く。目次にある項目のときだけ
logReading :: LogLine -> Card -> Either Mistake Card
logReading line card = do
  checkTarget (contents (work card)) (OnEntry (readEntry line))
  pure card { readLog = readLog card ++ [line] }
-- ANCHOR_END: logReading

-- ANCHOR: rateEntry
-- ⭐ Rate one entry, like an episode, if it is in the table
--    (a new rating replaces the old one, as with `rate`).
-- 目次の項目（話など）1つに評価を付ける。目次にある項目のときだけ
-- （付け直したら、rate と同じく、新しいほうにする）
rateEntry :: EntryId -> Score -> Card -> Either Mistake Card
rateEntry i r card = do
  checkTarget (contents (work card)) (OnEntry i)
  pure card { entryRatings = Map.insert i r (entryRatings card) }

-- 🧺 Every rating I gave this work: the whole work (if rated) and
--    each entry, from top to bottom.
-- この作品に付けた評価を全部：作品全体（付けていれば）と、
-- 項目ごとの評価（上から順）
ratingsOf :: Card -> [Score]
ratingsOf card = maybeToList (rating card) ++ Map.elems (entryRatings card)
-- ANCHOR_END: rateEntry
-- ANCHOR_END: all
