-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
-- 🚪 A tiny way in from the command line. It knows the window and the
--    views; it does not know where the cards are kept.
-- コマンドからの小さな入り口。窓口と見せ方は知っている。
-- カードがどこにしまわれているかは、知らない
module Main (main) where

import Control.Monad (forM_, void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import System.IO (hSetEncoding, stdout, utf8)

import Tsundoku.Card (Card (..))
import Tsundoku.Examples
import Tsundoku.Rating
import Tsundoku.Shelf
import Tsundoku.Shelf.Config
import Tsundoku.Types
-- ANCHOR_END: imports

-- ANCHOR: main
-- 🚪 TSUNDOKU_DB chooses the store (SQLite's tsundoku.db if unset).
-- TSUNDOKU_DB でしまう場所を選ぶ（なければ SQLite の tsundoku.db）
main :: IO ()
main = do
  hSetEncoding stdout utf8
  setting <- fromMaybe "sqlite:tsundoku.db" <$> lookupEnv "TSUNDOKU_DB"
  config  <- either die pure (parseStoreConfig setting)
  args    <- getArgs
  withShelf config $ \shelf -> case args of
    ["add-examples"] -> addExamples shelf
    ["list"]         -> listShelf shelf
    _                -> die "usage: shelf add-examples | shelf list"

-- 📚 Put three works on the shelf, through the window only.
-- 作品を3つ、本棚に置く。窓口だけを通して
addExamples :: Shelf -> IO ()
addExamples shelf = do
  dragon <- shelf.addWork dragonBook
  void (shelf.applyEvent dragon Start)
  forM_ (parse FiveStars "★4.5") (shelf.rate dragon)
  film <- shelf.addWork lastDragonFilm
  void (shelf.applyEvent film Start)
  void (shelf.applyEvent film Finish)
  _ <- shelf.addWork dungeonGame
  putStrLn "added 3 works"

-- 📋 One line for each card: number, state, rating, and title.
-- カード1枚に1行：番号、状態、評価、題
listShelf :: Shelf -> IO ()
listShelf shelf = do
  cards <- shelf.listCards
  forM_ cards $ \(WorkId n, card) ->
    T.putStrLn (T.intercalate "  "
      [ T.pack (show n) <> "."
      , stateLabel (reading card)
      , maybe "  -  " (render FiveStars) (rating card)
      , title (work card)
      ])
-- ANCHOR_END: main

-- ANCHOR: label
-- 🏷️ How the command line shows a reading state.
-- コマンドの画面での、読書の状態の見せ方
stateLabel :: ReadingState -> Text
stateLabel state = case state of
  Piled     -> "積読"
  Reading   -> "読書中"
  Finished  -> "読了"
  Paused    -> "中断"
  Rereading -> "再読"
-- ANCHOR_END: label
-- ANCHOR_END: all
