-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
-- | The window of the shelf: everything you can do with it, as a record
--   of functions. Every way in (the API, the web pages, MCP) uses only
--   this. Where the cards are kept is hidden behind it.
-- 本棚の窓口：できることの全部を、関数のレコードにしたもの。
-- 入り口（API・Web の画面・MCP）は、どれもこれだけを使う。
-- カードをどこにしまうかは、この向こうに隠れる
module Tsundoku.Shelf
  ( Shelf (..)
  , WorkWindow (..)
  , ReadingWindow (..)
  , CustomWindow (..)
  , IntakeWindow (..)
  , InsideWindow (..)
  , Glance (..)
  , glance
  , addChecked
  , decide
  , searchWith
  ) where

import Data.Map.Strict (Map)
import Data.Text       (Text)

import Tsundoku.Card   (Card, checkWork)
import Tsundoku.Rating (Score)
import Tsundoku.Search (Hit, Search, parseSearch)
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: shelf
-- 🪟 What you can do with the shelf, as small windows put together.
--    Each small window is one kind of thing to do; an entrance gets only
--    the windows it needs (shelf.works.addWork, shelf.reading.rate).
-- 本棚でできること。小さな窓口を組み合わせたもの。小さな窓口の1つ1つが、
-- することの種類1つ。入り口は、要る窓口だけをもらう
-- （shelf.works.addWork、shelf.reading.rate）
data Shelf = Shelf
  { works   :: WorkWindow     -- 📚 the works on the shelf / 本棚の作品
  , reading :: ReadingWindow  -- 📖 reading, notes, ratings / 読む・感想・評価
  , custom  :: CustomWindow   -- 🏷️ my own attributes, states / 自分で決める
  , intake  :: IntakeWindow   -- 📥 taking in from outside / 外から取り込む
  , inside  :: InsideWindow   -- 🔎 searching what is inside / 中身を探す
  }
-- ANCHOR_END: shelf

-- ANCHOR: windows
-- ANCHOR: workWindow
-- 📚 The works on the shelf.
-- 本棚の作品
data WorkWindow = WorkWindow
  { addWork    :: Work   -> IO WorkId
  , removeWork :: WorkId -> IO Bool
  , getCard    :: WorkId -> IO (Maybe Card)
  , listCards  ::           IO [(WorkId, Card)]
  , reviseWork :: WorkId -> Revision -> IO (Checked ())
  }
-- ANCHOR_END: workWindow

-- ANCHOR: readingWindow
-- 📖 Reading, notes, and ratings.
-- 読む・感想・評価
data ReadingWindow = ReadingWindow
  { applyEvent :: WorkId -> Event            -> IO (Checked ReadingState)
  , addNote    :: WorkId -> Note             -> IO (Checked ())
  , logReading :: WorkId -> LogLine          -> IO (Checked ())
  , unread     :: WorkId -> EntryId          -> IO (Checked ())
  , rate       :: WorkId -> Score            -> IO (Checked ())
  , rateEntry  :: WorkId -> EntryId -> Score -> IO (Checked ())
  }
-- ANCHOR_END: readingWindow

-- 🏷️ My own attributes and states.
-- 自分で決める属性と状態
data CustomWindow = CustomWindow
  { getSchema  ::                            IO Schema
  , defineAttr :: AttrDef                 -> IO (Checked ())
  , defineFlow :: Flow                    -> IO (Checked ())
  , editAttrs  :: WorkId -> Map Text Text -> IO (Checked ())
  , moveFlow   :: WorkId -> Text -> Text  -> IO (Checked ())
  , findCards  :: Text                    -> IO (Checked [(WorkId, Card)])
  }

-- 📥 Taking things in from outside.
-- 外から取り込む
data IntakeWindow = IntakeWindow
  { addEpisodes :: WorkId -> Int -> IO (Checked ())
  }

-- 🔎 Searching what is inside.
-- 中身を探す
data InsideWindow = InsideWindow
  { setText    :: WorkId -> EntryId -> Text -> IO (Checked ())
  , searchText :: Text                      -> IO (Checked [Hit])
  }
-- ANCHOR_END: windows

-- ANCHOR: glance
-- 👀 A window that can only look: made of the parts of other windows
--    that change nothing. Give an entrance only this, and its type says
--    it cannot change the shelf: it has no function that would.
-- 見るだけの窓口：ほかの窓口のうち、何も変えない部分で作る。入り口に
-- これだけを渡せば、本棚を変えられないことを型が言う（変える関数を、
-- そもそも持っていない）
data Glance = Glance
  { listCards  :: IO [(WorkId, Card)]
  , getCard    :: WorkId -> IO (Maybe Card)
  , searchText :: Text -> IO (Checked [Hit])
  }

glance :: Shelf -> Glance
glance shelf = Glance
  { listCards  = shelf.works.listCards
  , getCard    = shelf.works.getCard
  , searchText = shelf.inside.searchText
  }
-- ANCHOR_END: glance

-- ANCHOR: addChecked
-- 🚪 Add a work through the core's gate (checkWork). The window takes
--    any work; every entrance that takes works from outside (the API,
--    MCP) adds them through this.
-- 核の門（checkWork）を通して作品を足す。窓口はどんな作品も受け取るので、
-- 外から作品を受け取る入り口（API・MCP）は、どれもこれを通して足す
addChecked :: WorkWindow -> Work -> IO (Checked WorkId)
addChecked window work = case checkWork work of
  Left why  -> pure (Left why)
  Right ok  -> Right <$> window.addWork ok
-- ANCHOR_END: addChecked

-- ANCHOR: decide
-- 🧑‍⚖️ The core judges every change. No card: no such work. Otherwise
--    the core's function says yes (the new card) or no (why not).
-- 変えてよいかを決めるのは、いつも核。カードがなければ NoSuchWork。
-- あれば、核の関数が「よし」（新しいカード）か「だめ」（理由）を言う
decide :: WorkId -> (Card -> Checked Card) -> Maybe Card -> Checked Card
decide wid _    Nothing     = Left (NoSuchWork wid)
decide _   core (Just card) = core card
-- ANCHOR_END: decide

-- ANCHOR: searchWith
-- 🚪 Search through the gate. What was typed becomes a Search first, so
--    what cannot be searched never reaches the store.
-- 門番を通して探す。打ったものを先に Search にするので、探せないものは
-- しまう場所まで届かない
searchWith :: (Search -> IO [Hit]) -> Text -> IO (Checked [Hit])
searchWith store typed = case parseSearch typed of
  Left why     -> pure (Left why)
  Right search -> Right <$> store search
-- ANCHOR_END: searchWith
-- ANCHOR_END: all
