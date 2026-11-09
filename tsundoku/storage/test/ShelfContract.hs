-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
-- 📜 The contract: what every shelf must do, wherever it keeps cards.
-- 契約：本棚なら、カードをどこにしまっても、こう動かなければならない
module ShelfContract
  ( shelfContract
  , tornContract
  , tornWork
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Tree (Tree (..), flatten)
import Test.Hspec

import Tsundoku.Card (Card (..), newCard)
import Tsundoku.Examples
import Tsundoku.Rating (score)
import Tsundoku.Shelf
import Tsundoku.Types
-- ANCHOR_END: imports

-- ANCHOR: contract
-- 📜 The same tests for any shelf. Each test gets an empty shelf.
-- どの本棚にも同じテスト。テストごとに、空っぽの本棚を受け取る
shelfContract :: SpecWith Shelf
shelfContract = do

  -- 「空っぽから始まる」
  it "starts empty" $ \shelf -> do
    cards <- shelf.listCards
    map fst cards `shouldBe` []

  -- 「どの種類の作品も、しまったとおりに読み戻せる」
  forM_ examples $ \(kind, w) ->
    it ("gives back a new card of a " <> kind <> " as it was") $ \shelf -> do
      wid <- shelf.addWork w
      shelf.getCard wid `shouldReturn` Just (newCard w)

  -- 「番号は、しまった順に、前の続きから振る」
  it "numbers works and entries in order, going on" $ \shelf -> do
    first  <- shelf.addWork dragonBook
    second <- shelf.addWork librarianSerial
    [first, second] `shouldBe` [WorkId 1, WorkId 2]
    card <- fromJust <$> shelf.getCard second
    entryIds (contents (work card)) `shouldBe` map EntryId [9 .. 13]
    headingsOf (contents (work card))
      `shouldBe` headingsOf (contents librarianSerial)

  -- 「消した番号は、二度と使わない」
  it "never gives a removed number again" $ \shelf -> do
    old <- shelf.addWork dragonBook
    shelf.removeWork old `shouldReturn` True
    shelf.removeWork old `shouldReturn` False
    shelf.getCard old `shouldReturn` Nothing
    new  <- shelf.addWork dragonBook
    card <- fromJust <$> shelf.getCard new
    new `shouldBe` WorkId 2
    entryIds (contents (work card)) `shouldBe` map EntryId [9 .. 16]

  -- 「ない作品には、何もできない」
  it "refuses a work that is not on the shelf" $ \shelf -> do
    let nowhere = WorkId 42
        missing = Left (NoSuchWork nowhere)
    shelf.getCard nowhere `shouldReturn` Nothing
    shelf.applyEvent nowhere Start `shouldReturn` missing
    shelf.addNote nowhere (Note OnWork "!" (onDay 2026 11 9) False)
      `shouldReturn` missing
    shelf.logReading nowhere (LogLine (nov 9) (EntryId 1))
      `shouldReturn` missing
    shelf.rate nowhere (surely (score 80)) `shouldReturn` missing
    shelf.rateEntry nowhere (EntryId 1) (surely (score 80))
      `shouldReturn` missing

  -- 「読書の状態は、地図のとおりにだけ動く」
  it "moves only along the map of reading" $ \shelf -> do
    wid <- shelf.addWork lastDragonFilm
    shelf.applyEvent wid Start  `shouldReturn` Right Reading
    shelf.applyEvent wid Finish `shouldReturn` Right Finished
    shelf.applyEvent wid Start
      `shouldReturn` Left (NotAllowed Start Finished)
    fmap reading <$> shelf.getCard wid `shouldReturn` Just Finished

  -- 「核が断るものは、窓口も断る」
  it "refuses what the core refuses" $ \shelf -> do
    wid <- shelf.addWork dragonBook
    shelf.addNote wid (Note OnWork "  " (onDay 2026 11 9) False)
      `shouldReturn` Left EmptyNote
    shelf.addNote wid
        (Note (OnEntry (EntryId 99)) "?" (onDay 2026 11 9) False)
      `shouldReturn` Left (NoSuchEntry (EntryId 99))
    shelf.logReading wid (LogLine (nov 9) (EntryId 99))
      `shouldReturn` Left (NoSuchEntry (EntryId 99))
    shelf.rateEntry wid (EntryId 99) (surely (score 80))
      `shouldReturn` Left (NoSuchEntry (EntryId 99))
    fmap notes <$> shelf.getCard wid `shouldReturn` Just []

  -- 「付け直した評価は、新しいほう」
  it "keeps the newest rating" $ \shelf -> do
    wid <- shelf.addWork librarianSerial
    _   <- shelf.rate wid (surely (score 80))
    _   <- shelf.rate wid (surely (score 90))
    _   <- shelf.rateEntry wid (EntryId 2) (surely (score 70))
    _   <- shelf.rateEntry wid (EntryId 2) (surely (score 100))
    card <- fromJust <$> shelf.getCard wid
    rating card `shouldBe` Just (surely (score 90))
    Map.toList (entryRatings card)
      `shouldBe` [(EntryId 2, surely (score 100))]

  -- 「核が作ったカードと、窓口を通したカードは、同じ」
  it "keeps the dragon card just as the core made it" $ \shelf -> do
    wid <- shelf.addWork dragonBook
    _   <- shelf.applyEvent wid Start
    forM_ (notes dragonCard)   (shelf.addNote wid)
    forM_ (rating dragonCard)  (shelf.rate wid)
    forM_ (readLog dragonCard) (shelf.logReading wid)
    shelf.getCard wid `shouldReturn` Just dragonCard
  where
    examples =
      [ ("book",   dragonBook)
      , ("serial", copyistSerial)
      , ("series", detectiveDrama)
      , ("film",   lastDragonFilm)
      , ("game",   dungeonGame)
      ]
-- ANCHOR_END: contract

-- ANCHOR: headings
-- 📑 The headings of a work, from top to bottom (numbers left out).
-- 作品の目次の見出しを、上から順に（番号は見ない）
headingsOf :: Contents -> [Text]
headingsOf what = case what of
  Book   toc -> fromToc toc
  Serial toc -> fromToc toc
  Series toc -> fromToc toc
  Film   _   -> []
  Game       -> []
  where
    fromToc toc = map heading (concatMap flatten toc)
-- ANCHOR_END: headings

-- ANCHOR: torn
-- 🧨 A shelf in a database: a failure in the middle of a write leaves
--    nothing behind. The third entry has -1 pages, which the table's
--    CHECK refuses, after the work and two entries are in.
-- データベースの本棚：書いている途中で失敗したら、何も残らない。
-- 3つ目の項目が -1 ページ。作品と2つの項目を入れたあとで、表の CHECK が断る
tornContract :: SpecWith Shelf
tornContract =
  -- 「途中で転んだら、全部なかったことに」
  it "keeps all of a new work, or none of it" $ \shelf -> do
    result <- try (shelf.addWork tornWork)
    isLeft (result :: Either SomeException WorkId) `shouldBe` True
    cards <- shelf.listCards
    map fst cards `shouldBe` []

-- 📕 A book whose third entry has -1 pages.
-- 3つ目の項目が -1 ページの本
tornWork :: Work
tornWork = Work "破れた本" "だれか" (Book (numbered
  [ Node ("第1章", Pages 10)
      [ Node ("1.1", Pages 5)    []
      , Node ("1.2", Pages (-1)) []
      ]
  ]))
-- ANCHOR_END: torn
-- ANCHOR_END: all
