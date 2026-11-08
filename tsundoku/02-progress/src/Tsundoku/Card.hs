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
  ) where

-- `when` and text
-- when と文字列
import Control.Monad (when)
import Data.Text qualified as T

import Tsundoku.Rating (Score)
import Tsundoku.Reading (advance)
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: card
-- 🗂️ One card of the library: the work, where I am, my rating,
--    my notes, and my reading log (both oldest first).
-- 図書室の読書カード1枚：作品、いまの状態、評価、感想、読書ログ
-- （感想とログは古い順）
data Card = Card
  { work    :: Work
  , reading :: ReadingState
  , rating  :: Maybe Score
  , notes   :: [Note]
  , readLog :: [LogLine]
  }
  deriving (Show, Eq)

-- 🆕 A new card starts on the pile, with no rating, no notes,
--    and nothing in the log.
-- 新しいカードは、積読から。評価も感想も、ログもまだない
newCard :: Work -> Card
newCard w = Card
  { work    = w
  , reading = Piled
  , rating  = Nothing
  , notes   = []
  , readLog = []
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
-- ANCHOR_END: all
