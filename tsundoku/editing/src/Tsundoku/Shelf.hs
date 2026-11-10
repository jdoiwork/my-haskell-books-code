-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE NoFieldSelectors #-}
-- | The window of the shelf: everything you can do with it, as a record
--   of functions. Every way in (the API, the web pages, MCP) uses only
--   this. Where the cards are kept is hidden behind it.
-- 本棚の窓口：できることの全部を、関数のレコードにしたもの。
-- 入り口（API・Web の画面・MCP）は、どれもこれだけを使う。
-- カードをどこにしまうかは、この向こうに隠れる
module Tsundoku.Shelf
  ( Shelf (..)
  , decide
  , searchWith
  ) where

import Data.Map.Strict (Map)
import Data.Text       (Text)

import Tsundoku.Card   (Card)
import Tsundoku.Rating (Score)
import Tsundoku.Search (Hit, Search, parseSearch)
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: shelf
-- 🪟 What you can do with the shelf. Each field is one thing.
-- 本棚でできること。フィールドの1つ1つが、できることの1つ
data Shelf = Shelf
  { -- 📚 The works on the shelf
    -- 本棚の作品
    addWork     :: Work                       -> IO WorkId
  , removeWork  :: WorkId                     -> IO Bool
  , getCard     :: WorkId                     -> IO (Maybe Card)
  , listCards   ::                               IO [(WorkId, Card)]
  , reviseWork  :: WorkId -> Revision         -> IO (Checked ())

    -- ANCHOR: shelfReading
    -- 📖 Reading, notes, and ratings
    -- 読む・感想・評価
  , applyEvent  :: WorkId -> Event            -> IO (Checked ReadingState)
  , addNote     :: WorkId -> Note             -> IO (Checked ())
  , logReading  :: WorkId -> LogLine          -> IO (Checked ())
  , unread      :: WorkId -> EntryId          -> IO (Checked ())
  , rate        :: WorkId -> Score            -> IO (Checked ())
  , rateEntry   :: WorkId -> EntryId -> Score -> IO (Checked ())
    -- ANCHOR_END: shelfReading

    -- 🏷️ My own attributes and states
    -- 自分で決める属性と状態
  , getSchema   ::                               IO Schema
  , defineAttr  :: AttrDef                    -> IO (Checked ())
  , defineFlow  :: Flow                       -> IO (Checked ())
  , editAttrs   :: WorkId -> Map Text Text    -> IO (Checked ())
  , moveFlow    :: WorkId -> Text -> Text     -> IO (Checked ())
  , findCards   :: Text                       -> IO (Checked [(WorkId, Card)])

    -- 📥 Taking things in from outside
    -- 外から取り込む
  , addEpisodes :: WorkId -> Int              -> IO (Checked ())

    -- 🔎 Searching what is inside
    -- 中身を探す
  , setText     :: WorkId -> EntryId -> Text  -> IO (Checked ())
  , searchText  :: Text                       -> IO (Checked [Hit])
  }
-- ANCHOR_END: shelf

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
