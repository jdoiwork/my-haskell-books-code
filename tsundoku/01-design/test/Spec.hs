module Main (main) where

-- ANCHOR: imports
import Data.Either (isRight)
import Data.Text (Text)
import Data.Tree (Forest, flatten)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Tsundoku.Card
import Tsundoku.Examples
import Tsundoku.Rating
import Tsundoku.Reading
import Tsundoku.Types

import ShelfAnswers.TenthStars
-- ANCHOR_END: imports

-- ANCHOR: model
-- 🗺️ The map of reading, copied by hand from the picture in the
--    chapter, as plain triples (no `moves` here).
-- 読書の地図を、章の絵から手で写した、ただの3つ組のリスト（moves は使わない）
expectedMoves :: [(ReadingState, Event, ReadingState)]
expectedMoves =
  [ (Piled    , Start , Reading  )
  , (Reading  , Finish, Finished )
  , (Reading  , Stop  , Paused   )
  , (Paused   , Resume, Reading  )
  , (Finished , Reread, Rereading)
  , (Rereading, Finish, Finished )
  , (Rereading, Stop  , Finished )
  ]

-- 📋 Every value of a small type, like all states or all events.
-- 小さな型の値を全部（状態の全部、できごとの全部など）
everything :: (Bounded a, Enum a) => [a]
everything = [minBound .. maxBound]

-- 🚶 Try events one by one from a state. A refused event changes
--    nothing. Gives every state on the way, the first one too.
-- 状態から、できごとを1つずつ試す。断られたら、そのまま。
-- 通った状態を、最初のものも含めて全部返す
walk :: ReadingState -> [Event] -> [ReadingState]
walk = scanl step
  where
    step state event = either (const state) id (advance event state)
-- ANCHOR_END: model

-- 🌳 Outlines of any shape, with plain titles.
-- どんな形でもよい下書き（題はなんでもよい）
outline :: Forest Int -> Forest (Text, Int)
outline = map (fmap (\n -> ("entry", n)))

-- ANCHOR: spec
main :: IO ()
main = hspec $ do
  -- 「★と点は、範囲の中でしか作れない」
  describe "stars and scores" $ do
    -- 「★6、★3.3、-3 点、101 点は作れない」
    it "refuses values out of range" $ do
      stars 6     `shouldBe` Left (BadStars 6)
      stars 3.3   `shouldBe` Left (BadStars 3.3)
      score (-3)  `shouldBe` Left (BadScore (-3))
      score 101   `shouldBe` Left (BadScore 101)
    -- 「25 点は★1.5（真ん中は切り上げ）、24 点は★1」
    it "rounds halves up" $ do
      fmap (starsValue . scoreToStars) (score 25) `shouldBe` Right 1.5
      fmap (starsValue . scoreToStars) (score 24) `shouldBe` Right 1
    -- 「1〜5 の 0.5 刻みだけを受け付ける」
    prop "accepts exactly 1 to 5 by halves" $
      forAll (choose (-4, 24 :: Int)) $ \quarters ->
        let x = fromIntegral quarters / 4
        in  isRight (stars x)
              === (quarters >= 4 && quarters <= 20 && even quarters)
    -- 「★を点にして戻すと、元の★」
    it "goes from stars to a score and back without loss" $ do
      length allStars `shouldBe` 9
      [ scoreToStars (starsToScore s) | s <- allStars ] `shouldBe` allStars
    -- 「点を★にして戻しても、20〜100 点。15 点からは、ずれて 5 点まで」
    prop "goes from a score to stars and back, staying close" $
      forAll (choose (0, 100)) $ \n ->
        let back = scoreValue (starsToScore (scoreToStars (surely (score n))))
        in  back >= 20 && back <= 100 && (n < 15 || abs (back - n) <= 5)

  -- 「読書の状態は、地図のとおりに進む」
  describe "the map of reading" $ do
    -- 「状態とできごとの組、25 通りすべてが、手で写した地図のとおり」
    it "does what the map says, for every state and event" $
      sequence_
        [ advance event state `shouldBe` expected
        | state <- everything
        , event <- everything
        , let expected =
                case [ to | (from, e, to) <- expectedMoves
                          , from == state, e == event ] of
                  [to] -> Right to
                  _    -> Left (NotAllowed event state)
        ]
    -- 「一度、積読を出たら、積読には戻らない」
    prop "never goes back to the pile" $
      forAll (listOf (elements everything)) $ \events ->
        let afterPile = dropWhile (== Piled) (walk Piled events)
        in  Piled `notElem` afterPile
    -- 「一度、読了したら、読了か再読のまま」
    prop "once finished, stays finished or rereading" $
      forAll (listOf (elements everything)) $ \events ->
        let states      = walk Piled events
            afterFinish = dropWhile (/= Finished) states
        in  checkCoverage
              $ cover 40 (not (null afterFinish)) "reaches Finished"
              $ all (`elem` [Finished, Rereading]) afterFinish

  -- 「目次と感想」
  describe "tables and notes" $ do
    -- 「番号は、上から順に 1, 2, 3...」
    prop "numbers an outline 1, 2, 3... from top to bottom" $
      \shape ->
        let toc = numbered (outline shape)
            n   = length (concatMap flatten shape)
        in  map entryId (concatMap flatten toc) === map EntryId [1 .. n]
    -- 「1行の版も、同じ番号を振る」
    prop "numbers the same way in one line" $
      \shape -> numberedShort (outline shape) === numbered (outline shape)
    -- 「ない項目や、空の感想は断る」
    it "refuses a note on a missing entry, or with nothing in it" $ do
      let card = newCard dragonBook
          note = Note (OnEntry (EntryId 99)) "?" (onDay 2026 11 9) False
      addNote note card `shouldBe` Left (NoSuchEntry (EntryId 99))
      addNote note { body = "  " } card `shouldBe` Left EmptyNote
      addNote note { target = OnEntry (EntryId 1) } (newCard lastDragonFilm)
        `shouldBe` Left (NoSuchEntry (EntryId 1))
    -- 「感想は、古い順に並ぶ」
    it "keeps notes oldest first" $
      map body (notes dragonCard)
        `shouldBe` ["絵が多くて読みやすい", "火を吐かない種もいるとは！"]
    -- 「ネタバレは、読み終えた人にだけ見せる」
    it "shows spoilers only after the end" $ do
      length (visibleTo Reading  (notes dragonCard)) `shouldBe` 1
      length (visibleTo Finished (notes dragonCard)) `shouldBe` 2

  -- 「トレーニングの解答」
  describe "answers" $ do
    -- 「0.1 刻みの★は、1.0〜5.0 の41通りを全部受け付け、3.35 は断る」
    it "accepts every tenth from 1.0 to 5.0, and nothing between" $ do
      let tenthsOk =
            [ tenthStars (fromIntegral k / 10) | k <- [10 .. 50 :: Int] ]
      length [ () | Right _ <- tenthsOk ] `shouldBe` 41
      tenthStars 3.35 `shouldBe` Left (BadStars 3.35)
      fmap tenthStarsValue (tenthStars 1.1) `shouldBe` Right 1.1
    -- 「25 点は★1.3、★1.3 は 26 点」
    it "rounds points to the nearest tenth" $
      fmap (tenthToPoints . pointsToTenth) (score 25) `shouldBe` Right 26
-- ANCHOR_END: spec
