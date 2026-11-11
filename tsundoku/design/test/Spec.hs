module Main (main) where

-- ANCHOR: imports
import Data.Either (isRight)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Forest, flatten)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Tsundoku.Card
import Tsundoku.Examples
import Tsundoku.Rating
import Tsundoku.Reading
import Tsundoku.Types

import ShelfAnswers.Grades
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
  , (Finished , Reread, Reading  )
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
walk = scanl tryOne
  where
    tryOne state event = either (const state) id (advance event state)
-- ANCHOR_END: model

-- ANCHOR: outline
-- 🌳 Outlines of any shape, with plain titles.
-- どんな形でもよい下書き（題はなんでもよい）
outline :: Forest Int -> Forest (Text, Int)
outline = map (fmap (\n -> ("entry", n)))
-- ANCHOR_END: outline

-- ANCHOR: spec
main :: IO ()
main = hspec $ do
  -- 「評価のモデルは1つ。見せ方は、その影」
  describe "one score, many styles" $ do
    -- 「-3 点、101 点は作れない」
    it "refuses scores out of range" $ do
      score (-3) `shouldBe` Left (BadScore (-3))
      score 101  `shouldBe` Left (BadScore 101)
    -- 「73 点を、4つの見せ方で見せる」
    it "shows one score in every style" $
      fmap (\s -> map (`render` s) allStyles) (score 73)
        `shouldBe` Right ["73点", "★3.5", "7/10", "7.3/10"]
    -- 「真ん中は切り上げ。75 点は★4、25 点は★1.5、4 点は★0」
    it "rounds halves up" $ do
      fmap (render FiveStars) (score 75) `shouldBe` Right "★4"
      fmap (render FiveStars) (score 25) `shouldBe` Right "★1.5"
      fmap (render FiveStars) (score 4)  `shouldBe` Right "★0"
      fmap (render (OutOfTen NoDecimals)) (score 75) `shouldBe` Right "8/10"
    -- 「小数1けたの10点満点は、80 点でも 8.0」
    it "always writes one decimal place out of ten" $
      fmap (render (OutOfTen OneDecimal)) (score 80) `shouldBe` Right "8.0/10"
    -- 「打ちこんだ評価を読み戻す。印は省いてもよい」
    it "reads what was typed" $ do
      parse FiveStars "★4.5"   `shouldBe` score 90
      parse FiveStars " 4.5 "  `shouldBe` score 90
      parse FiveStars "★6"     `shouldBe` Left (BadRating "★6")
      parse FiveStars "★3.3"   `shouldBe` Left (BadRating "★3.3")
      parse Hundred "-3"       `shouldBe` Left (BadRating "-3")
      parse Hundred "73点"     `shouldBe` score 73
      parse (OutOfTen NoDecimals) "7.5" `shouldBe` Left (BadRating "7.5")
      parse (OutOfTen OneDecimal) "7.5" `shouldBe` score 75
      parse (OutOfTen OneDecimal) "8.0/10" `shouldBe` score 80
    -- 「★は、0〜5 の 0.5 刻みだけを受け付ける（0.0 から 6.0 まで全部）」
    it "accepts stars from 0 to 5 by halves, and nothing else" $
      sequence_
        [ (typed, isRight (parse FiveStars typed))
            `shouldBe` (typed, t <= 50 && t `mod` 5 == 0)
        | t <- [0 .. 60 :: Int]
        , let typed = T.pack (show (t `div` 10) ++ "." ++ show (t `mod` 10))
        ]
    -- 「その見せ方で表せる点なら、見せて読み戻すと元の点（全部の組で）」
    it "gives back every score a style can show" $
      sequence_
        [ (style, parse style (render style (surely (score n))))
            `shouldBe` (style, score n)
        | style <- allStyles
        , n     <- [0, step style .. 100]
        ]
    -- 「影は、元の点から目盛りの半分までしか離れず、影の影は同じ影
    --   （見せ方4つと点 101 通りの組、404 通り全部で）」
    it "casts a shadow close to the score, for every style and score" $
      sequence_
        [ do (style, n, abs (scoreValue shadow - n) <= step style `div` 2)
               `shouldBe` (style, n, True)
             (style, n, render style shadow)
               `shouldBe` (style, n, render style model)
        | style <- allStyles
        , n     <- [0 .. 100]
        , let model  = surely (score n)
              shadow = surely (parse style (render style model))
        ]

  -- 「読書の状態は、地図のとおりに進む」
  describe "the map of reading" $ do
    -- 「状態とできごとの組、20 通りすべてが、手で写した地図のとおり」
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
    -- 「読了から出るのは、読み返して読書中に戻るときだけ」
    prop "leaves finished only by rereading, back to reading" $
      forAll (listOf (elements everything)) $ \events ->
        let states = walk Piled events
            leaves = [ next | (Finished, next) <- zip states (drop 1 states)
                            , next /= Finished ]
        in  checkCoverage
              $ cover 40 (Finished `elem` states) "reaches Finished"
              $ all (== Reading) leaves

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
    -- 「ネタバレは、読み終えた人にだけ見せる（状態4つを全部）」
    it "shows spoilers only after the end, for every state" $
      [ (state, map body (visibleTo state (notes dragonCard)))
      | state <- everything ]
        `shouldBe`
          [ (Piled   , ["絵が多くて読みやすい"])
          , (Reading , ["絵が多くて読みやすい"])
          , (Finished, ["絵が多くて読みやすい", "火を吐かない種もいるとは！"])
          , (Paused  , ["絵が多くて読みやすい"])
          ]
    -- 「できごとは、カードの状態だけを進める。起こせなければ断る」
    it "moves a card on the map, and changes nothing else" $ do
      applyEvent Finish dragonCard
        `shouldBe` Right dragonCard { reading = Finished }
      applyEvent Start dragonCard
        `shouldBe` Left (NotAllowed Start Reading)
    -- 「評価を付けると、評価だけが変わる。付け直したら、新しいほう」
    it "rates a card, and changes nothing else" $ do
      let seventy = surely (score 70)
      rating dragonCard `shouldBe` Just (surely (score 90))
      rate seventy dragonCard
        `shouldBe` dragonCard { rating = Just seventy }

  -- 「トレーニングの解答」
  describe "answers" $ do
    -- 「成績で見せて読み戻すと、同じ成績。モデルは変えずに足せた」
    it "adds grades without touching the model" $ do
      fmap renderGrade (score 73) `shouldBe` Right "C"
      fmap renderGrade (score 59) `shouldBe` Right "F"
      [ fmap renderGrade (parseGrade g) | g <- ["A", "B", "C", "D", "F"] ]
        `shouldBe` map Right ["A", "B", "C", "D", "F"]
      parseGrade "E" `shouldBe` Left (BadRating "E")
-- ANCHOR_END: spec
