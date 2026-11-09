-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedRecordDot #-}
-- | The review of Part 12: a reading circle. Several people read the same
--   book on their own shelves, and meet to talk. Who is where? Whose notes
--   may I read without spoilers? Up to where can we all talk today?
--   Everything comes from the cards (the core) of the tsundoku package.
-- 第12部のふりかえり：読書会。何人かが、同じ本を自分の本棚で読み、集まって
-- 話す。だれがどこまで？ だれの感想なら、ネタバレなしに読める？ 今日は、
-- どこまでの話をしてよい？ 全部、tsundoku のカード（核）から出す
module Review.BookClub
  ( Member (..)
  , progressLines
  , seenBy
  , ahead
  , meetingPoint
  , sampleClub
  ) where

import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Tree (flatten)

import Tsundoku.Bar (cardSummary)
import Tsundoku.Card (Card (..), addNote)
import Tsundoku.Examples (dragonCard, finishedDragon, onDay, surely)
import Tsundoku.Progress (finishedIn)
import Tsundoku.Review (Seen (..), seenNotes)
import Tsundoku.Shelf.Rows (toPlain)
import Tsundoku.Tui.View (padTo)
import Tsundoku.Types
-- ANCHOR_END: head

-- ANCHOR: member
-- 👥 One person of the circle, with their own card of the same book.
-- 読書会の1人。同じ本の、その人のカードを持つ
data Member = Member
  { name :: Text
  , card :: Card
  }

-- 📊 Where everyone is: a name, then the bar of their own card. Names
--    are padded by their width on the screen (padTo counts a full-width
--    letter as two), so the bars line up.
-- みんながどこまで：名前と、その人のカードのバー。名前は画面の幅で詰める
-- （padTo は全角の文字を2桁と数える）ので、バーがそろう
progressLines :: [Member] -> [Text]
progressLines members =
  [ padTo 12 m.name <> fromMaybe "（進み具合なし）" (cardSummary 10 m.card)
  | m <- members ]
-- ANCHOR_END: member

-- ANCHOR: seenBy
-- 👀 The notes of the writer, as the reader may see them now. The core's
--    seenNotes already says what a card may see of its own notes; to look
--    with the reader's eyes, give the writer's notes the reader's log and
--    reading state.
-- 書いた人の感想を、読む人がいま見てよい形で。核の seenNotes は、カードが
-- 自分の感想をどう見てよいかを言う。読む人の目で見るには、書いた人の感想に、
-- 読む人のログと読書の状態を持たせればよい
seenBy :: Card -> Card -> [Seen]
seenBy reader writer =
  seenNotes writer { readLog = reader.readLog, reading = reader.reading }
-- ANCHOR_END: seenBy

-- ANCHOR: ahead
-- 🏃 The entries one has read and the other has not yet, in the order of
--    the table: what the first should not talk about yet.
-- 一方が読んで、もう一方がまだの項目。目次の順で。前の人が、まだ話しては
-- いけないところ
ahead :: Card -> Card -> [Text]
ahead front back =
  [ h | (e, h) <- entriesOf front, e `Set.member` mine, not (e `Set.member` theirs) ]
  where
    mine   = finishedIn front.readLog
    theirs = finishedIn back.readLog

-- 🌳 Every entry of a work, from the top, with its heading.
-- 作品の項目の全部を、上から順に、見出しと一緒に
entriesOf :: Card -> [(EntryId, Text)]
entriesOf c =
  let (_, _, toc) = toPlain c.work.contents
  in  [ (entryId e, heading e) | e <- concatMap flatten toc ]
-- ANCHOR_END: ahead

-- ANCHOR: meetingPoint
-- 📍 Up to where everyone has read, without a gap: the last entry of the
--    longest beginning of the table that all members have finished.
--    Nothing if someone has not read even the first entry.
-- みんなが、すき間なく読んだところまで：全員が読み終えた、目次の頭からの
-- いちばん長い部分の、最後の項目。だれかが最初の項目も読んでいなければ
-- Nothing
meetingPoint :: [Member] -> Maybe Text
meetingPoint members = case members of
  []    -> Nothing
  m : _ -> case reverse (takeWhile everyone (entriesOf m.card)) of
    (_, h) : _ -> Just h
    []         -> Nothing
  where
    everyone (e, _) = all (\x -> e `Set.member` finishedIn x.card.readLog) members
-- ANCHOR_END: meetingPoint

-- ANCHOR: sampleClub
-- 📚 A small circle on the dragon book. ラムダくん has finished it and
--    wrote a spoiler on the last chapter; TS くん has read up to entry 5.
-- ドラゴン学の小さな読書会。ラムダくんは読み終えて、最後の章にネタバレを
-- 書いた。TS くんは項目 5 まで読んだ
sampleClub :: [Member]
sampleClub =
  [ Member "ラムダくん" lambda
  , Member "TS くん"   dragonCard ]
  where
    lambda = surely $ addNote
      (Note (OnEntry (EntryId 8)) "最後は、竜と和解する" (onDay 2026 11 12) True)
      finishedDragon
-- ANCHOR_END: sampleClub
-- ANCHOR_END: all
