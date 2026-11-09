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
import System.Exit (die, exitFailure)
import System.IO (hSetEncoding, stderr, stdout, utf8)

import Tsundoku.Card (Card (..))
import Tsundoku.Examples
import Tsundoku.Form (explain)
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
  hSetEncoding stderr utf8
  setting <- fromMaybe "sqlite:tsundoku.db" <$> lookupEnv "TSUNDOKU_DB"
  config  <- either die pure (parseStoreConfig setting)
  args    <- getArgs
  withShelf config $ \shelf -> case args of
    ["add-examples"] -> addExamples shelf
    ["list"]         -> listShelf shelf
    ["find", typed]  -> findCards shelf (T.pack typed)
    _                -> die usage
  where
    usage = "usage: shelf add-examples | shelf list | shelf find SEARCH"

-- 📚 Put five works on the shelf, through the window only: first what
--    the library defined, then the works and what I wrote on them.
-- 作品を5つ、本棚に置く。窓口だけを通して：まず図書室が決めたもの、
-- それから作品と、そこに書いたもの
addExamples :: Shelf -> IO ()
addExamples shelf = do
  mapM_ shelf.defineAttr libraryAttrs
  mapM_ shelf.defineFlow libraryFlows
  dragon <- shelf.addWork dragonBook
  void (shelf.applyEvent dragon Start)
  forM_ (parse FiveStars "★4.5") (shelf.rate dragon)
  void (shelf.editAttrs dragon dragonForm)
  void (shelf.moveFlow dragon "貸し出し" "本棚にある")
  serial <- shelf.addWork librarianSerial
  void (shelf.applyEvent serial Start)
  forM_ (parse FiveStars "★5") (shelf.rate serial)
  void (shelf.editAttrs serial librarianForm)
  void (shelf.moveFlow serial "連載の状態" "連載中")
  copyist <- shelf.addWork copyistSerial
  void (shelf.applyEvent copyist Start)
  forM_ (parse FiveStars "★4") (shelf.rate copyist)
  void (shelf.editAttrs copyist copyistForm)
  void (shelf.moveFlow copyist "連載の状態" "連載中")
  void (shelf.moveFlow copyist "連載の状態" "休止")
  film <- shelf.addWork lastDragonFilm
  void (shelf.applyEvent film Start)
  void (shelf.applyEvent film Finish)
  void (shelf.addWork dungeonGame)
  putStrLn "added 5 works"

-- 📋 One line for each card.
-- カード1枚に1行
listShelf :: Shelf -> IO ()
listShelf shelf = do
  cards <- shelf.listCards
  mapM_ (T.putStrLn . cardLine) cards

-- 🔎 The cards a search finds, or why it could not search.
-- 絞り込みで見つかったカード。絞り込めなければ、その理由
findCards :: Shelf -> Text -> IO ()
findCards shelf typed = do
  found <- shelf.findCards typed
  case found of
    Left why    -> do
      mapM_ (T.hPutStrLn stderr) (explain why)
      exitFailure
    Right cards -> mapM_ (T.putStrLn . cardLine) cards

-- 📋 A card in one line: number, state, rating, and title.
-- カードを1行に：番号、状態、評価、題
cardLine :: (WorkId, Card) -> Text
cardLine (WorkId n, card) = T.intercalate "  "
  [ T.pack (show n) <> "."
  , stateLabel (reading card)
  , maybe "  -  " (render FiveStars) (rating card)
  , title (work card)
  ]
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
