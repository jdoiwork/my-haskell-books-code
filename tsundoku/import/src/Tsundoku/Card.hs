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
  , editAttrs
  , stateOf
  , moveFlow
  , episodesIn
  , addEpisodes
  ) where

-- `when`, dictionaries, Maybe, and text
-- when、辞書、Maybe、文字列
import Control.Monad (when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (flatten)

import Tsundoku.Custom (findFlow, nextStates, readFields)
import Tsundoku.Progress (growWith)
import Tsundoku.Rating (Score)
import Tsundoku.Reading (advance)
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: card
-- 🗂️ One card of the library: the work, where I am, my rating of
--    the whole work, my notes, my reading log (both oldest first),
--    my ratings of single entries, like episodes, and the values of
--    my own attributes and states (by their names).
-- 図書室の読書カード1枚：作品、いまの状態、作品全体の評価、感想、
-- 読書ログ（感想とログは古い順）、目次の項目（話など）ごとの評価、
-- 自分で決めた属性の値と状態（名前で引く）
data Card = Card
  { work         :: Work
  , reading      :: ReadingState
  , rating       :: Maybe Score
  , notes        :: [Note]
  , readLog      :: [LogLine]
  , entryRatings :: Map EntryId Score
  , attrs        :: Map Text AttrValue
  , states       :: Map Text Text
  }
  deriving (Show, Eq)

-- 🆕 A new card starts on the pile, with no rating, no notes,
--    nothing in the log, and none of my own attributes.
-- 新しいカードは、積読から。評価も感想も、ログも、自分で決めた属性も
-- まだない
newCard :: Work -> Card
newCard w = Card
  { work         = w
  , reading      = Piled
  , rating       = Nothing
  , notes        = []
  , readLog      = []
  , entryRatings = Map.empty
  , attrs        = Map.empty
  , states       = Map.empty
  }
-- ANCHOR_END: card

-- ANCHOR: addNote
-- ✍️ Add a note, if it says something and is about something real.
-- 感想を足す。中身があって、ちゃんとある所に付くときだけ
addNote :: Note -> Card -> Checked Card
addNote note card = do
  when (T.null (T.strip (body note))) (Left EmptyNote)
  checkTarget (contents (work card)) (target note)
  pure card { notes = notes card ++ [note] }

-- 🎯 The whole work is always there; an entry must be in the table.
-- 作品全体は、いつでもある。目次の項目は、目次になければならない
checkTarget :: Contents -> Target -> Checked ()
checkTarget _    OnWork = Right ()
checkTarget what (OnEntry i)
  | i `elem` entryIds what = Right ()
  | otherwise              = Left (NoSuchEntry i)
-- ANCHOR_END: addNote

-- ANCHOR: applyEvent
-- 🔔 Something happened: move the card on the map of reading.
-- できごとが起きた：カードを、読書の地図の上で進める
applyEvent :: Event -> Card -> Checked Card
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
  | state == Finished = allNotes
  | otherwise         = [ note | note <- allNotes, not (spoiler note) ]
-- ANCHOR_END: visibleTo

-- ANCHOR: logReading
-- 📓 Write a line in the reading log, if the entry is in the table.
-- 読書ログに1行書く。目次にある項目のときだけ
logReading :: LogLine -> Card -> Checked Card
logReading line card = do
  checkTarget (contents (work card)) (OnEntry (readEntry line))
  pure card { readLog = readLog card ++ [line] }
-- ANCHOR_END: logReading

-- ANCHOR: rateEntry
-- ⭐ Rate one entry, like an episode, if it is in the table
--    (a new rating replaces the old one, as with `rate`).
-- 目次の項目（話など）1つに評価を付ける。目次にある項目のときだけ
-- （付け直したら、rate と同じく、新しいほうにする）
rateEntry :: EntryId -> Score -> Card -> Checked Card
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

-- ANCHOR: editAttrs
-- 🏷️ Fill in my own attributes from a form, if every field reads by
--    its definition. A blank field takes the value away; a field that
--    is not in the form stays as it was.
-- フォームから、自分で決めた属性を書きこむ。どの欄も定義どおりに
-- 読めたときだけ。空の欄は値を消す。フォームにない欄は、そのまま
editAttrs :: Map Text Text -> Schema -> Card -> Checked Card
editAttrs typed schema card = do
  fields <- readFields schema typed
  pure card { attrs = Map.foldrWithKey put (attrs card) fields }
  where
    put name Nothing      = Map.delete name
    put name (Just value) = Map.insert name value
-- ANCHOR_END: editAttrs

-- ANCHOR: moveFlow
-- 📍 Where a card is in a flow, if it is in it at all.
-- カードが、流れのどこにいるか。まだ入っていなければ Nothing
stateOf :: Flow -> Card -> Maybe Text
stateOf flow card = Map.lookup (flowName flow) (states card)

-- 🔀 Move a card to a state of a flow: first to the start, then along
--    the arrows.
-- カードを、流れの状態へ動かす。はじめは、はじめの状態へ。
-- そのあとは、矢印に沿って
moveFlow :: Text -> Text -> Schema -> Card -> Checked Card
moveFlow name to schema card = do
  flow <- findFlow schema name
  let from = stateOf flow card
  when (to `notElem` nextStates flow from) (Left (NoSuchArrow name from to))
  pure card { states = Map.insert name to (states card) }
-- ANCHOR_END: moveFlow

-- ANCHOR: addEpisodes
-- 🔢 How many episodes a serial has. Arcs count 0, episodes 1.
-- 連載に、何話あるか。章は 0、話は 1 と数える
episodesIn :: Toc Episodes -> Int
episodesIn toc = sum [ n | Entry _ _ (Episodes n) <- concatMap flatten toc ]

-- 🌱 New episodes came out: one for each number the shelf gave, named
--    第4話, 第5話, ... (the API tells how many, not their titles).
-- 新しい話が出た：本棚がくれた番号ごとに1話、第4話、第5話……と名前を
-- 付けて足す（API が教えてくれるのは話の数だけで、題はわからない）
addEpisodes :: [EntryId] -> Card -> Checked Card
addEpisodes ids card = case contents (work card) of
  Serial toc -> Right card { work = (work card) { contents = grown toc } }
  Book   _   -> Left NotASerial
  Series _   -> Left NotASerial
  Film   _   -> Left NotASerial
  Game       -> Left NotASerial
  where
    grown toc = Serial (foldl' add toc (zip [episodesIn toc + 1 ..] ids))
    add toc (n, eid) =
      growWith eid ("第" <> T.pack (show n) <> "話", Episodes 1) toc
-- ANCHOR_END: addEpisodes
-- ANCHOR_END: all
