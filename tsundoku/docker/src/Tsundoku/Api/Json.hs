-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE TemplateHaskell       #-}
-- | What goes over the wire: the shapes of the API's JSON. They are the
--   promise to the screens, kept apart from the core's types, so that
--   renaming something in the core never changes the JSON by accident.
--   Only plain things: numbers, words, lists. Nothing about how to draw.
-- 線の上を行き来するもの：API の JSON の形。画面との約束なので、核の型とは
-- 分けておく（核で名前を変えても、JSON がうっかり変わらないように）。
-- 中身は素朴なものだけ：数、言葉、リスト。描き方のことは何も持たない
module Tsundoku.Api.Json
  ( -- * Going out / 出ていくもの
    WorkSummary (..)
  , ProgressNumbers (..)
  , CardDetail (..)
  , TocItem (..)
  , NoteItem (..)
  , ReadingNow (..)
  , HitItem (..)
  , Snip (..)
  , Problem (..)
  , FormField (..)
  , summaryOf
  , detailOf
  , noteItem
  , readingNow
  , hitItem
  , formField
    -- * Coming in / 入ってくるもの
  , NewWork (..)
  , Outline (..)
  , EditedWork (..)
  , OpenedWork (..)
  , EditedItem (..)
  , NewNote (..)
  , Happened (..)
  , ReadOn (..)
  , Points (..)
  , workFrom
  , asNewWork
  , revisionFrom
  , noteFrom
  , logFrom
    -- * Words / 言葉
  , nameOf
  , fromName
  ) where

-- dictionaries, sets, text, time, and trees
-- 辞書、集合、文字列、日時、木
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, toGregorian)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Tree (Tree (..))
import GHC.Generics (Generic)

-- JSON, and TypeScript types made from the same types
-- JSON と、同じ型から作る TypeScript の型
import Data.Aeson (FromJSON, ToJSON, defaultOptions)
import Data.Aeson.TypeScript.TH (deriveTypeScript)

import Tsundoku.Card (Card (..))
import Tsundoku.Form (FormRow (..), Widget (..))
import Tsundoku.Progress (Progress (..), finishedIn, percent, progressOf)
import Tsundoku.Rating (scoreValue)
import Tsundoku.Reading (allowed)
import Tsundoku.Search (Hit (..), Piece (..), Place (..))
import Tsundoku.Shelf.Rows (fromPlain, toPlain)
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: summary
-- 📇 One line of the shelf: enough for a list, nothing more.
--    The rating is the score (0 to 100), never stars.
-- 本棚の1行：一覧に要るものだけ。評価は点（0〜100）で、★にはしない
data WorkSummary = WorkSummary
  { id       :: Int
  , title    :: Text
  , creator  :: Text
  , kind     :: Text               -- "book", "serial", "series", ...
  , reading  :: Text               -- "Piled", "Reading", ...
  , rating   :: Maybe Int          -- 0..100
  , progress :: Maybe ProgressNumbers
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 📊 How far, as numbers. The bar, its length and its colour, are
--    the screen's business; the rounding of the percent is the core's.
-- 進み具合を、数で。バーの長さや色は画面の仕事。割合の丸め方は核の仕事
data ProgressNumbers = ProgressNumbers
  { done    :: Int
  , total   :: Int
  , unit    :: Text                -- "pages", "episodes", "minutes"
  , percent :: Int
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
-- ANCHOR_END: summary

-- ANCHOR: detail
-- 🗂️ One whole card: the summary, the events I may do now (a screen
--    can make its buttons from them), the table, and the notes.
-- カード1枚の全部：一覧の1行、いまできること（画面はここから
-- ボタンを作れる）、目次、感想
data CardDetail = CardDetail
  { work    :: WorkSummary
  , next    :: [Text]              -- "Start", "Finish", ...
  , minutes :: Maybe Int           -- the length of a film / 映画の長さ
  , toc     :: [TocItem]
  , notes   :: [NoteItem]
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 🌳 One entry of the table, with its children. `size` is its own
--    amount; `done` says whether I have read it to the end.
-- 目次の1項目と、その子。size はその項目だけの量。done は読み終えたか
data TocItem = TocItem
  { id       :: Int
  , heading  :: Text
  , size     :: Int
  , done     :: Bool
  , rating   :: Maybe Int
  , children :: [TocItem]
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 📝 A note. Spoilers come too, marked: whether to fold them away
--    is up to the screen.
-- 感想。ネタバレも、印を付けて返す。畳んで隠すかどうかは、画面が決める
data NoteItem = NoteItem
  { entry     :: Maybe Int         -- none: about the whole work / 作品全体
  , body      :: Text
  , writtenAt :: Text              -- "2026-11-09T12:00:00Z"
  , spoiler   :: Bool
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 🔔 Where I am now, and what I may do next.
-- いまの状態と、次にできること
data ReadingNow = ReadingNow
  { reading :: Text
  , next    :: [Text]
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
-- ANCHOR_END: detail

-- ANCHOR: hit
-- 🔎 One hit of a search. The snippet is a list of pieces, each
--    marked if it is one of the words. <mark> or 【】 is the screen's.
-- 検索で見つかったもの1つ。抜き出しはかけらのリストで、探した語には
-- 印が付く。<mark> にするか【】にするかは、画面が決める
data HitItem = HitItem
  { work    :: Int
  , title   :: Text
  , place   :: Text                -- "title", "creator", "heading", ...
  , entry   :: Maybe Int
  , spoiler :: Bool
  , trail   :: [Text]
  , snippet :: [Snip]
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- ✂️ A piece of a snippet: its text, and whether it is a word I looked for.
-- 抜き出しのかけら：文字と、探した語かどうか
data Snip = Snip
  { text  :: Text
  , match :: Bool
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
-- ANCHOR_END: hit

-- ANCHOR: problem
-- 🚫 Why the API said no. `mistake` is the word a program can rely on;
--    `messages` are words for a person, which a screen may replace.
-- API が断った理由。mistake はプログラムが頼ってよい言葉。messages は
-- 人に向けた言葉で、画面が自分の言葉に置きかえてもよい
data Problem = Problem
  { mistake  :: Text               -- "no-such-work", "bad-score", ...
  , messages :: [Text]
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
-- ANCHOR_END: problem

-- ANCHOR: formField
-- 🎛️ One row of the form of my own attributes and states, for any
--    screen. `kind` says what to draw; `choices` are the choices, or the
--    next states of a flow.
-- 自分で決めた属性と状態のフォームの1行。どの画面のためにも。kind は
-- 描くもの、choices は選択肢か、流れの次の状態
data FormField = FormField
  { label   :: Text
  , kind    :: Text      -- "text" "number" "date" "yesno" "choice" "flow"
  , choices :: [Text]
  , current :: Text
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 🎛️ A row of Tsundoku.Form, onto the wire. Each widget has its word.
-- Tsundoku.Form の行を、線の上へ。描くものごとに言葉がある
formField :: FormRow -> FormField
formField row = case row.widget of
  TextBox    -> as "text"   []
  NumberBox  -> as "number" []
  DateBox    -> as "date"   []
  CheckBox   -> as "yesno"  []
  Select cs  -> as "choice" cs
  Buttons ns -> as "flow"   ns
  where
    as word cs = FormField
      { label = row.label, kind = word, choices = cs, current = row.current }
-- ANCHOR_END: formField

-- ANCHOR: incoming
-- 📥 A new work, as it comes in. The numbers of the table are the
--    shelf's to give, so the outline has none.
-- 入ってくる新しい作品。目次の番号は本棚が振るので、下書きには番号がない
data NewWork = NewWork
  { title   :: Text
  , creator :: Text
  , kind    :: Text                -- "book", "serial", "series", ...
  , minutes :: Maybe Int           -- for a film / 映画なら
  , toc     :: [Outline]
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 🌿 One heading of the outline, its own amount, and its children.
-- 下書きの見出し1つと、その量、子
data Outline = Outline
  { heading  :: Text
  , size     :: Int
  , children :: [Outline]
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- ANCHOR: edited
-- ✏️ A work as fixed in the edit screen: its title, its creator, and
--    the whole table as it should be from now on. What it is (a book,
--    a serial, ...) cannot be changed here. The notes on lines that are
--    gone move to the whole work ("move") or go with them ("drop").
--    With it comes the work as the screen opened it (opened): the shelf
--    fixes the work only if it is still just like that.
-- 直す画面で直した作品：題、作者、これからの目次の全部。種類（本・連載
-- ……）は、ここでは変えられない。消える行の感想は、作品全体へ移すか
-- （"move"）、一緒に消すか（"drop"）。画面を開いたときの作品（opened）も
-- 一緒に送る。本棚は、作品がいまもそのままのときだけ直す
data EditedWork = EditedWork
  { title        :: Text
  , creator      :: Text
  , toc          :: [EditedItem]
  , removedNotes :: Text  -- "move" or "drop" / 移すか、消すか
  , opened       :: OpenedWork
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 📸 The work as the edit screen saw it when it opened: the title, the
--    creator, and the table, every line with its number.
-- 直す画面が開いたときに見た作品：題、作者、目次（どの行も番号つき）
data OpenedWork = OpenedWork
  { title   :: Text
  , creator :: Text
  , toc     :: [EditedItem]
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 🌿 One line of the fixed table. A line that was there sends its
--    number back (id); a new line sends null.
-- 直した目次の1行。もとからある行は、その番号（id）を送り返す。
-- 新しい行は null を送る
data EditedItem = EditedItem
  { id       :: Maybe Int
  , heading  :: Text
  , size     :: Int
  , children :: [EditedItem]
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
-- ANCHOR_END: edited

-- ✍️ A new note. The time is the server's clock, not the screen's.
-- 新しい感想。書いた時刻は、画面ではなくサーバーの時計で付ける
data NewNote = NewNote
  { entry   :: Maybe Int
  , body    :: Text
  , spoiler :: Bool
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 🔔 Something I did with a work: "Start", "Finish", ...
-- 作品に対してしたこと："Start"、"Finish"……
newtype Happened = Happened { event :: Text }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 📓 On this day, I read this entry to the end. Dates go over the wire
--    as plain text, as in ISO 8601: "2026-11-09".
-- この日に、この項目を読み終えた。日付は、ISO 8601 の素朴な文字で
-- 行き来する："2026-11-09"
data ReadOn = ReadOn
  { day   :: Text
  , entry :: Int
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- ⭐ A rating, always as points from 0 to 100.
-- 評価。いつも 0〜100 の点で
newtype Points = Points { points :: Int }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
-- ANCHOR_END: incoming

-- ANCHOR: words
-- 🏷️ Reading states and events go over the wire by the names of
--    their constructors: "Piled", "Start", ... (as in the tables).
-- 読書の状態とできごとは、コンストラクターの名前で行き来する：
-- "Piled"、"Start"……（表の中と同じ）
nameOf :: Show a => a -> Text
nameOf = T.pack . show

-- 🔎 And back. Only the names of the constructors are read.
-- そして、もとに戻す。読めるのはコンストラクターの名前だけ
fromName :: (Show a, Enum a, Bounded a) => Text -> Maybe a
fromName name = lookup name [ (nameOf x, x) | x <- [minBound .. maxBound] ]
-- ANCHOR_END: words

-- ANCHOR: summaryOf
-- 📇 A card as one line of the shelf.
-- カードを、本棚の1行に
summaryOf :: (WorkId, Card) -> WorkSummary
summaryOf (WorkId wid, card) = WorkSummary
  { id       = wid
  , title    = card.work.title
  , creator  = card.work.creator
  , kind     = kindWord
  , reading  = nameOf card.reading
  , rating   = scoreValue <$> card.rating
  , progress = progressNumbers card
  }
  where
    (kindWord, _, _) = toPlain card.work.contents

-- 📊 The progress of a card, in the unit of its kind. A film and a
--    game have no table, so no progress.
-- カードの進み具合を、種類ごとの単位で。映画とゲームは目次がないので、なし
progressNumbers :: Card -> Maybe ProgressNumbers
progressNumbers card = case card.work.contents of
  Book   toc -> Just (numbersIn "pages"    toc)
  Serial toc -> Just (numbersIn "episodes" toc)
  Series toc -> Just (numbersIn "minutes"  toc)
  Film   _   -> Nothing
  Game       -> Nothing
  where
    numbersIn :: Amount u => Text -> Toc u -> ProgressNumbers
    numbersIn unitWord toc =
      let p = progressOf (finishedIn card.readLog) toc
      in  ProgressNumbers
            { done    = count p.done
            , total   = count p.total
            , unit    = unitWord
            , percent = percent p
            }
-- ANCHOR_END: summaryOf

-- ANCHOR: detailOf
-- 🗂️ A whole card, for one work.
-- 作品1つの、カードの全部
detailOf :: (WorkId, Card) -> CardDetail
detailOf (wid, card) = CardDetail
  { work    = summaryOf (wid, card)
  , next    = map nameOf (allowed card.reading)
  , minutes = filmMinutes
  , toc     = map (tocItem finished card) plainToc
  , notes   = map noteItem card.notes
  }
  where
    (_, filmMinutes, plainToc) = toPlain card.work.contents
    finished                   = finishedIn card.readLog

-- 🌳 One entry of the table (amounts already plain numbers).
-- 目次の1項目（量は、もう素朴な数）
tocItem :: Set EntryId -> Card -> Tree (Entry Int) -> TocItem
tocItem finished card (Node e kids) = TocItem
  { id       = n
  , heading  = e.heading
  , size     = e.size
  , done     = e.entryId `Set.member` finished
  , rating   = scoreValue <$> Map.lookup e.entryId card.entryRatings
  , children = map (tocItem finished card) kids
  }
  where
    EntryId n = e.entryId

-- 📝 A note, with the number of its entry (if it is about one).
-- 感想を、付いている項目の番号と一緒に
noteItem :: Note -> NoteItem
noteItem note = NoteItem
  { entry     = entryNumber note.target
  , body      = note.body
  , writtenAt = T.pack (iso8601Show note.writtenAt)
  , spoiler   = note.spoiler
  }
  where
    entryNumber OnWork                = Nothing
    entryNumber (OnEntry (EntryId i)) = Just i

-- 🔔 Where I am, and what I may do next.
-- いまの状態と、次にできること
readingNow :: ReadingState -> ReadingNow
readingNow now = ReadingNow
  { reading = nameOf now
  , next    = map nameOf (allowed now)
  }
-- ANCHOR_END: detailOf

-- ANCHOR: hitItem
-- 🔎 A hit, with its place as a word and its snippet as pieces.
-- 見つかったものを、場所は言葉に、抜き出しはかけらにして
hitItem :: Hit -> HitItem
hitItem hit = HitItem
  { work    = wid
  , title   = hit.hitTitle
  , place   = placeWord
  , entry   = entryNumber
  , spoiler = isSpoiler
  , trail   = hit.trail
  , snippet = map snip hit.snippet
  }
  where
    WorkId wid = hit.hitWork
    (placeWord, entryNumber, isSpoiler) = case hit.place of
      InTitle                         -> ("title",   Nothing, False)
      InCreator                       -> ("creator", Nothing, False)
      InHeading (EntryId i)           -> ("heading", Just i,  False)
      InNote OnWork              s    -> ("note",    Nothing, s)
      InNote (OnEntry (EntryId i)) s  -> ("note",    Just i,  s)
      InText (EntryId i)              -> ("text",    Just i,  False)
    snip (Plain t) = Snip { text = t, match = False }
    snip (Match t) = Snip { text = t, match = True }
-- ANCHOR_END: hitItem

-- ANCHOR: workFrom
-- 📥 A new work from what came in. Nothing if the kind is not one of
--    the words we know ("book", "serial", "series", "film", "game").
--    Whether the work itself is all right is the core's to say (checkWork).
-- 入ってきたものから、新しい作品を。種類が知っている言葉（"book"・
-- "serial"・"series"・"film"・"game"）でなければ Nothing。作品として
-- よいかどうかは、核が言う（checkWork）
workFrom :: NewWork -> Maybe Work
workFrom new = do
  what <- fromPlain new.kind new.minutes (numbered (map draft new.toc))
  pure Work { title = new.title, creator = new.creator, contents = what }
  where
    draft o = Node (o.heading, o.size) (map draft o.children)

-- 📤 And the other way: a work as it would come in (for a client).
--    The numbers of the table are dropped; the shelf gives new ones.
-- 逆向き：作品を、入ってくるときの形に（クライアントのため）。
-- 目次の番号は落とす。本棚が新しく振るので
asNewWork :: Work -> NewWork
asNewWork w = NewWork
  { title   = w.title
  , creator = w.creator
  , kind    = kindWord
  , minutes = filmMinutes
  , toc     = map outlineOf plainToc
  }
  where
    (kindWord, filmMinutes, plainToc) = toPlain w.contents
    outlineOf (Node e kids) = Outline
      { heading  = e.heading
      , size     = e.size
      , children = map outlineOf kids
      }

-- ANCHOR: revisionFrom
-- ✏️ The core's revision, from what the edit screen sent.
-- 直す画面が送ってきたものから、核の Revision を
revisionFrom :: EditedWork -> Revision
revisionFrom edited = Revision
  { newTitle       = edited.title
  , newCreator     = edited.creator
  , newToc         = map line edited.toc
  , notesOnRemoved = choice edited.removedNotes
  , basedOn        = Opened
      { openedTitle   = edited.opened.title
      , openedCreator = edited.opened.creator
      , openedToc     = map line edited.opened.toc
      }
  }
  where
    -- Only a plain "drop" drops; anything else takes the safe way.
    -- はっきり "drop" のときだけ消す。それ以外は、安全な方へ
    choice "drop" = DropNotes
    choice _      = MoveToWork
    line i = Node (Item (EntryId <$> i.id) i.heading i.size)
                  (map line i.children)
-- ANCHOR_END: revisionFrom

-- ✍️ A note, written now.
-- いま書いた感想
noteFrom :: UTCTime -> NewNote -> Note
noteFrom now new = Note
  { target    = maybe OnWork (OnEntry . EntryId) new.entry
  , body      = new.body
  , writtenAt = now
  , spoiler   = new.spoiler
  }

-- 📓 A line of the reading log. Nothing if the day cannot be read,
--    or its year is not 1 to 9999 ("-0001-01-01" reads as a Day,
--    but SQLite cannot read it back, and PostgreSQL will not keep it).
-- 読書ログの1行。日付が読めないか、年が 1〜9999 でなければ Nothing
-- （"-0001-01-01" も Day としては読めるが、SQLite は読み戻せず、
-- PostgreSQL はしまえない）
logFrom :: ReadOn -> Maybe LogLine
logFrom r = do
  d <- iso8601ParseM (T.unpack r.day)
  let (year, _, _) = toGregorian d
  if year < 1 || year > 9999
    then Nothing
    else pure LogLine { readOn = d, readEntry = EntryId r.entry }
-- ANCHOR_END: workFrom

-- ANCHOR: typescript
-- 🟦 TypeScript types for every shape above, with the same options as
--    the JSON (aeson's defaults), so the names always agree. One splice
--    for all of them, so the order does not matter.
-- 上の形の全部に、TypeScript の型を。JSON と同じ設定（aeson の既定）
-- なので、名前は必ずそろう。全部を1つの式でまとめて作るので、順番は
-- 気にしなくてよい
$(concat <$> traverse (deriveTypeScript defaultOptions)
    [ ''WorkSummary, ''ProgressNumbers, ''CardDetail, ''TocItem
    , ''NoteItem, ''ReadingNow, ''HitItem, ''Snip, ''Problem
    , ''NewWork, ''Outline, ''NewNote, ''Happened, ''ReadOn, ''Points
    , ''FormField, ''EditedWork, ''OpenedWork, ''EditedItem
    ])
-- ANCHOR_END: typescript
-- ANCHOR_END: all
