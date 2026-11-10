-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedRecordDot #-}
-- | Training, step 4: move a whole shelf to another store, through the
--   two windows only.
-- トレーニングのステップ4：本棚をまるごと別の場所へ引っ越す。
-- 2つの窓口だけを使って
module ShelfAnswers.Move
  ( moveShelf
  , copyCard
  , pathTo
  , moveFile
  ) where

import Control.Monad (forM_, void)
import Data.Map.Strict qualified as Map

import Tsundoku.Card (Card (..))
import Tsundoku.Shelf
import Tsundoku.Shelf.Config
import Tsundoku.Types
-- ANCHOR_END: head

-- ANCHOR: move
-- 🚚 Copy every card of one shelf to another. Gives old and new numbers.
-- 本棚のカードを全部、別の本棚へ写す。古い番号と新しい番号を返す
moveShelf :: Shelf -> Shelf -> IO [(WorkId, WorkId)]
moveShelf from to = do
  cards <- from.works.listCards
  traverse (\(old, card) -> (,) old <$> copyCard to card) cards

-- 📇 Copy one card. The new shelf gives new numbers, so every entry
--    number in the card is changed to its new one (same place in the
--    table, top to bottom).
-- カードを1枚写す。新しい本棚は新しい番号を振るので、カードの中の項目の
-- 番号を、新しい番号に付け替える（目次の同じ場所、上から順）
copyCard :: Shelf -> Card -> IO WorkId
copyCard to card = do
  wid  <- to.works.addWork (work card)
  made <- to.works.getCard wid
  let newIds = maybe [] (entryIds . contents . work) made
      table  = Map.fromList (zip (entryIds (contents (work card))) newIds)
      moved i = Map.findWithDefault i i table
  forM_ (pathTo (reading card)) (void . to.reading.applyEvent wid)
  forM_ (notes card) $ \n ->
    to.reading.addNote wid n { target = onEntry moved (target n) }
  forM_ (readLog card) $ \l ->
    to.reading.logReading wid l { readEntry = moved (readEntry l) }
  forM_ (rating card) (to.reading.rate wid)
  forM_ (Map.toList (entryRatings card)) $ \(i, s) ->
    to.reading.rateEntry wid (moved i) s
  pure wid
  where
    onEntry _     OnWork      = OnWork
    onEntry moved (OnEntry i) = OnEntry (moved i)

-- 🗺️ The events that take a new card (on the pile) to a state.
-- 新しいカード（積読）を、その状態まで連れていくできごと
pathTo :: ReadingState -> [Event]
pathTo state = case state of
  Piled     -> []
  Reading   -> [Start]
  Finished  -> [Start, Finish]
  Paused    -> [Start, Stop]
-- ANCHOR_END: move

-- ANCHOR: moveFile
-- 🐘 Move a SQLite file to PostgreSQL: open both, and hand them over.
-- SQLite のファイルを PostgreSQL へ引っ越す：2つを開いて、渡すだけ
moveFile :: FilePath -> String -> IO [(WorkId, WorkId)]
moveFile path url =
  withShelf (UseSqlite path) $ \from ->
    withShelf (UsePostgres url) $ \to ->
      moveShelf from to
-- ANCHOR_END: moveFile
-- ANCHOR_END: all
