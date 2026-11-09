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
  ) where

import Tsundoku.Card   (Card)
import Tsundoku.Rating (Score)
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: shelf
-- 🪟 What you can do with the shelf. Each field is one thing.
-- 本棚でできること。フィールドの1つ1つが、できることの1つ
data Shelf = Shelf
  { -- 📚 The works on the shelf
    -- 本棚の作品
    addWork    :: Work                       -> IO WorkId
  , removeWork :: WorkId                     -> IO Bool
  , getCard    :: WorkId                     -> IO (Maybe Card)
  , listCards  ::                               IO [(WorkId, Card)]

    -- 📖 Reading, notes, and ratings
    -- 読む・感想・評価
  , applyEvent :: WorkId -> Event            -> IO (Checked ReadingState)
  , addNote    :: WorkId -> Note             -> IO (Checked ())
  , logReading :: WorkId -> LogLine          -> IO (Checked ())
  , rate       :: WorkId -> Score            -> IO (Checked ())
  , rateEntry  :: WorkId -> EntryId -> Score -> IO (Checked ())
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
-- ANCHOR_END: all
