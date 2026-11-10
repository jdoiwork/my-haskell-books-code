module Main (main) where

-- ANCHOR: imports
import Control.Monad (foldM)
import Data.Either (isRight)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (diffDays)
import Data.Tree (Forest, Tree (..), flatten)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (total)

import Tsundoku.Bar
import Tsundoku.Card
import Tsundoku.Chart
import Tsundoku.Examples
import Tsundoku.Progress
import Tsundoku.Ranking
import Tsundoku.Rating
import Tsundoku.Reading
import Tsundoku.Types

import ShelfAnswers.ByKind
import ShelfAnswers.ColorBar
import ShelfAnswers.FirstUnread
import ShelfAnswers.Grades
import ShelfAnswers.HalfRead
import ShelfAnswers.Recent
import ShelfAnswers.Weights

-- ANCHOR: shelfImport
import ApiSpec
import HtmlSpec
import TuiSpec
import McpSpec
import ReviewSpec
import EditSpec
import CustomSpec
import ImportSpec
import SearchSpec
import ShelfSpec
-- ANCHOR_END: shelfImport
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

-- 🌳 Outlines of any shape, with plain titles.
-- どんな形でもよい下書き（題はなんでもよい）
outline :: Forest Int -> Forest (Text, Int)
outline = map (fmap (\n -> ("entry", n)))

-- ANCHOR: helpers
-- 📄 A table of any shape, with sizes of 0 pages or more.
-- どんな形でもよい目次。量は 0 ページ以上
pagesToc :: Forest Int -> Toc Pages
pagesToc shape = numbered (map (fmap (\n -> ("entry", Pages (abs n)))) shape)

-- 📏 Progress from two plain numbers, for short tests.
-- ただの数2つから進み具合を作る（短いテストのため）
pages :: Int -> Int -> Progress Pages
pages d t = Progress (Pages d) (Pages t)

-- 🎲 Some of the entries of a table, chosen at random, as read.
-- 目次の項目のいくつかを、でたらめに選んで、読んだことにする
someRead :: Toc u -> Gen (Set.Set EntryId)
someRead toc =
  Set.fromList <$> sublistOf (map entryId (concatMap flatten toc))
-- ANCHOR_END: helpers

-- ANCHOR: ratingHelpers
-- ⭐ Any numbers as ratings: folded into 0 to 100.
-- どんな数も、0〜100 に折りたたんで評価にする
scoresOf :: [Int] -> [Score]
scoresOf = map (\n -> surely (score (abs n `mod` 101)))

-- 📱 A serial called `name`, one episode per number, each episode
--    rated with that number.
-- name という題の連載。数1つにつき1話で、各話にその数の評価を付ける
ratedSerial :: Text -> [Int] -> Card
ratedSerial name ns = surely (foldM rateOne (newCard serial) numberedNs)
  where
    serial     = Work name "test" (Serial toc)
    toc        = numbered [ Node ("episode", Episodes 1) [] | _ <- ns ]
    numberedNs = zip (map EntryId [1 ..]) ns
    rateOne card (i, n) = do
      given <- score n
      rateEntry i given card

-- 🏆 The titles of a ranking, from the top.
-- ランキングの題を、上から
titles :: [(k, Card)] -> [Text]
titles = map (title . work . snd)

-- ➗ The plain average, by sum and length (not by `mean`).
-- ただの平均を、合計と個数で（mean を使わずに）
plainMean :: [Int] -> Rational
plainMean xs = toRational (sum ns) / toRational (length ns)
  where
    ns = map scoreValue (scoresOf xs)

-- 📐 The plain share of good ratings (0 when there are none).
-- 「よかった」の、ただの割合（評価がなければ 0）
share :: Int -> Int -> Double
share _    0 = 0
share good n = fromIntegral good / fromIntegral n
-- ANCHOR_END: ratingHelpers

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
    -- 「★は、0〜5 の 0.5 刻みだけを受け付ける」
    prop "accepts stars from 0 to 5 by halves, and nothing else" $
      forAll (choose (0, 60 :: Int)) $ \t ->
        let typed = T.pack (show (t `div` 10) ++ "." ++ show (t `mod` 10))
        in  isRight (parse FiveStars typed) === (t <= 50 && t `mod` 5 == 0)
    -- 「その見せ方で表せる点なら、見せて読み戻すと元の点」
    prop "gives back a score the style can show" $
      forAll (elements allStyles) $ \style ->
      forAll (fmap (* step style) (choose (0, 100 `div` step style))) $ \n ->
        parse style (render style (surely (score n))) === score n
    -- 「影は、元の点から目盛りの半分までしか離れず、影の影は同じ影」
    prop "casts a shadow close to the score" $
      forAll (elements allStyles) $ \style ->
      forAll (choose (0, 100)) $ \n ->
        let model  = surely (score n)
            shadow = surely (parse style (render style model))
        in  abs (scoreValue shadow - n) <= step style `div` 2
              && render style shadow == render style model

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
    -- 「ネタバレは、読み終えた人にだけ見せる」
    it "shows spoilers only after the end" $ do
      length (visibleTo Reading  (notes dragonCard)) `shouldBe` 1
      length (visibleTo Finished (notes dragonCard)) `shouldBe` 2

  -- ANCHOR: progressSpec
  -- 「進み具合は、モノイド」
  describe "progress is a monoid" $ do
    -- 「どこにカッコを付けても、足した結果は同じ（結合法則）」
    prop "adds up the same, wherever the parentheses go" $
      \(a, b) (c, d) (e, f) ->
        (pages a b <> pages c d) <> pages e f
          === pages a b <> (pages c d <> pages e f)
    -- 「何もない進み具合は、足しても何も変えない」
    prop "mempty changes nothing" $
      \(a, b) ->
        (mempty <> pages a b, pages a b <> mempty)
          === (pages a b, pages a b)

  -- 「目次を畳んで、進み具合にする」
  describe "folding a table" $ do
    -- 「ドラゴン学の本は、78 ページのうち 48 ページで 62%」
    it "reads 48 of the 78 pages of the dragon book" $ do
      let p = progressOf (finishedIn dragonLog) dragonToc
      p `shouldBe` pages 48 78
      percent p `shouldBe` 62
      remaining p `shouldBe` Pages 30
    -- 「木を畳んだ答えは、上から順に並べて素朴に足した答えと同じ」
    prop "gives what a plain sum over the entries gives" $
      \shape ->
        let toc   = pagesToc shape
            sizes = map abs (concatMap flatten shape)
        in  forAll (someRead toc) $ \finished ->
              let readSizes = [ n | (i, n) <- zip [1 ..] sizes
                                  , EntryId i `Set.member` finished ]
              in  progressOf finished toc
                    === pages (sum readSizes) (sum sizes)
    -- 「森をどこで2つに分けて畳んでも、足せば同じ」
    prop "gives the same, wherever the forest is split" $
      \shape ->
        let toc = pagesToc shape
        in  forAll (someRead toc) $ \finished ->
            forAll (choose (0, length toc)) $ \k ->
              let (front, back) = splitAt k toc
              in  progressOf finished toc
                    === progressOf finished front <> progressOf finished back
    -- 「百分率は 0〜100。100 は読み終えたときだけ、0 は読み始める前だけ」
    prop "is 0 to 100, 100 only at the end, 0 only before the start" $
      forAll (choose (1, 1000)) $ \t ->
      forAll (choose (0, t)) $ \d ->
        let pct = percent (pages d t)
        in  0 <= pct && pct <= 100
              && (pct == 100) == (d == t)
              && (pct == 0) == (d == 0)

  -- 「進捗バー」
  describe "bars" $ do
    -- 「ドラゴン学の本のバー」
    it "draws the bar of the dragon book" $ do
      let p = progressOf (finishedIn dragonLog) dragonToc
      bar 10 p `shouldBe` "▓▓▓▓▓▓░░░░  62%"
      summaryLine 10 p `shouldBe` "▓▓▓▓▓▓░░░░  62%  48/78ページ"
    -- 「バーが満タンになるのは、読み終えたときだけ」
    prop "is full only at the end" $
      forAll (choose (1, 40)) $ \width ->
      forAll (choose (1, 1000)) $ \t ->
      forAll (choose (0, t)) $ \d ->
        let cells = T.length (T.filter (== '▓') (bar width (pages d t)))
        in  (cells == width) === (d == t)
    -- 「目次に、項目ごとのバーを並べる」
    it "puts a bar on every entry of the table" $
      tocBars 6 (finishedIn dragonLog) dragonToc `shouldBe`
        [ "▓▓▓▓▓▓ 100%  第1部 ドラゴンを知る"
        , "▓▓▓▓▓▓ 100%    第1章 見分け方"
        , "▓▓▓▓▓▓ 100%      1.1 うろこの色"
        , "▓▓▓▓▓▓ 100%      1.2 火を吐くもの"
        , "▓▓▓▓▓▓ 100%    第2章 すみかと季節"
        , "░░░░░░   0%  第2部 ドラゴンと話す"
        , "░░░░░░   0%    第3章 あいさつの作法"
        , "░░░░░░   0%    第4章 怒らせたときは"
        ]

  -- 「しおりと、育つ目次」
  describe "bookmarks and growing tables" $ do
    -- 「昨日は 100%、新しい話が出た今日は 75%」
    it "goes from 100% to 75% when a new episode comes out" $ do
      let finished = surely (readThrough (EntryId 5) librarianToc)
          today    = grow ("第4話 返却ポストの怪", Episodes 1) librarianToc
      percent (progressOf finished librarianToc) `shouldBe` 100
      percent (progressOf finished today) `shouldBe` 75
      unreadBadge (progressOf finished today) `shouldBe` Just "未読 1話"
      unreadBadge (progressOf finished librarianToc) `shouldBe` Nothing
    -- 「目次にない番号には、しおりをはさめない」
    it "refuses a bookmark on a missing entry" $
      readThrough (EntryId 99) librarianToc
        `shouldBe` Left (NoSuchEntry (EntryId 99))
    -- 「育っても、前からある項目と番号はそのまま。新しい番号は初めて」
    prop "keeps every old entry and gives a new number" $
      \shape (NonNegative n) ->
        let old   = pagesToc shape
            new   = grow ("new", Pages n) old
            fresh = nextEntryId old
            olds  = concatMap flatten old
        in  fresh `notElem` map entryId olds
              && filter ((/= fresh) . entryId) (concatMap flatten new) == olds
              && total (progressOf Set.empty new)
                   == total (progressOf Set.empty old) <> Pages n

  -- 「読書ログと、ペース」
  describe "the reading log and the pace" $ do
    -- 「11月6日から8日まで、3日続けて読んだ」
    it "counts the days in a row" $ do
      let days = Map.keysSet (perDay dragonToc dragonLog)
      streak (nov 9) days `shouldBe` 3   -- not yet today / 今日はまだ
      streak (nov 8) days `shouldBe` 3
      streak (nov 2) days `shouldBe` 2
      streak (nov 5) days `shouldBe` 0
    -- 「最近7日に 42 ページ。残り 30 ページは、5日後の11月14日に読み終わる」
    it "finishes the dragon book on November 14" $ do
      let week = lastWeek (nov 9) (perDay dragonToc dragonLog)
          left = remaining (progressOf (finishedIn dragonLog) dragonToc)
      week `shouldBe` Pages 42
      finishBy (nov 9) left week `shouldBe` Just (nov 14)
    -- 「予測の日は、そのペースで読み終わる、いちばん早い日」
    prop "gives the first day the pace is enough" $
      forAll (choose (1, 500)) $ \l ->
      forAll (choose (1, 500)) $ \w ->
        case finishBy (nov 9) (Pages l) (Pages w) of
          Nothing  -> property False
          Just day ->
            let n = diffDays day (nov 9)
            in  property (n * toInteger w >= 7 * toInteger l
                            && (n - 1) * toInteger w < 7 * toInteger l)
    -- 「読み終えていたら今日、最近読んでいなければ Nothing」
    it "says today when done, and Nothing without a pace" $ do
      finishBy (nov 9) (Pages 0) (Pages 0) `shouldBe` Just (nov 9)
      finishBy (nov 9) (Pages 30) (Pages 0) `shouldBe` Nothing
    -- 「カードの読書ログと、1行のまとめ」
    it "logs reading on a card, and sums it up in one line" $ do
      logReading (LogLine (nov 9) (EntryId 99)) dragonCard
        `shouldBe` Left (NoSuchEntry (EntryId 99))
      cardSummary 10 dragonCard
        `shouldBe` Just "▓▓▓▓▓▓░░░░  62%  48/78ページ"
      cardSummary 10 (newCard lastDragonFilm) `shouldBe` Nothing
  -- ANCHOR_END: progressSpec

  -- ANCHOR: rankingSpec
  -- 「件数と合計は、足せる」
  describe "tallies" $ do
    -- 「リストをつないでから数えても、別々に数えて足しても同じ」
    prop "add up like the lists they came from" $ \xs ys ->
      tally (scoresOf xs ++ scoresOf ys)
        === tally (scoresOf xs) <> tally (scoresOf ys)
    -- 「結合法則」
    prop "are associative" $ \xs ys zs ->
      let a = tally (scoresOf xs)
          b = tally (scoresOf ys)
          c = tally (scoresOf zs)
      in  (a <> b) <> c === a <> (b <> c)
    -- 「平均は、いちばん低い評価と、いちばん高い評価のあいだ」
    prop "give an average between the lowest and the highest" $ \xs ->
      let given  = map (toRational . scoreValue) (scoresOf xs)
          between m = minimum given <= m && m <= maximum given
      in  maybe (null given) between (mean (tally (scoresOf xs)))

  -- 「ベイズ平均」
  describe "the Bayesian average" $ do
    -- 「ふつうの点と、その作品の平均のあいだにいる」
    prop "sits between the usual points and the work's own average" $
      \(NonNegative c) u (NonEmpty xs) ->
        let usualPts = toRational (abs u `mod` 101 :: Int)
            t        = tally (scoresOf xs)
            own      = plainMean xs
            b        = bayesian (Prior c usualPts) t
        in  min usualPts own <= b && b <= max usualPts own
    -- 「件数が増えるほど、その作品の平均に近づく（離れはしない）」
    prop "gets closer to the work's own average as ratings grow" $
      \(Positive c) u (NonEmpty xs) (Positive k) ->
        let prior      = Prior c (toRational (abs u `mod` 101 :: Int))
            own        = plainMean xs
            gap times  = abs (bayesian prior
                                (tally (concat (replicate times
                                                          (scoresOf xs))))
                              - own)
        in  gap (k + 1) <= gap k && gap k <= gap 0
    -- 「何件ぶんが 0 なら、ふつうの平均と同じ」
    prop "is the plain average when the weight is 0" $ \u (NonEmpty xs) ->
      let t = tally (scoresOf xs)
      in  Just (bayesian (Prior 0 (toRational (u :: Int))) t) === mean t
    -- 「全部の評価を同じだけ上げても、順位は変わらない」
    prop "keeps the order when every rating goes up by the same amount" $
      forAll (choose (1, 30)) $ \up ->
      forAll (listOf (listOf (choose (0, 100 - up)))) $ \shelf ->
      forAll (choose (0, 10)) $ \c ->
        let cards ns = [ ratedSerial (T.pack (show i)) row
                       | (i, row) <- zip [1 :: Int ..] ns ]
            order ns = titles (rankCards (bayesian (shelfPrior c (cards ns))
                                          . tally)
                                         (cards ns))
        in  order (map (map (+ up)) shelf) === order shelf
    -- 「わたしの本棚：ふつうの平均なら1話だけの★5が1位、
    --   ベイズ平均なら30話の連載が1位」
    it "puts 30 episodes above a single ★5 on my shelf" $ do
      take 2 (titles (rankCards (mean . tally) myShelf))
        `shouldBe` ["図書室の司書に転生した件", "迷宮図書館の写本係"]
      titles (rankCards (bayesian (shelfPrior 5 myShelf) . tally) myShelf)
        `shouldBe` [ "迷宮図書館の写本係", "図書室の司書に転生した件"
                   , "はじめてのドラゴン学", "王都の路地裏探偵"
                   , "最後のドラゴン" ]
    -- 「ふつうの点は、1作品1票。まだ評価していない作品は数えない」
    it "takes the usual points one vote per work" $ do
      usual (shelfPrior 5 myShelf) `shouldBe` 422 / 5
      usualPoints [] `shouldBe` 50

  -- 「Wilson スコアの下限」
  describe "the Wilson lower bound" $ do
    -- 「0 と、よかった割合のあいだ」
    prop "is between 0 and the share of good ratings" $
      \(NonNegative a) (NonNegative b) ->
        let w = wilsonLower (a, a + b)
        in  0 <= w && w <= share a (a + b) + 1e-12
    -- 「同じ割合で件数が倍になれば、下限は上がる（下がらない）」
    prop "rises when the same share has twice the ratings" $
      \(NonNegative a) (Positive n) ->
        let good = min a n
        in  wilsonLower (2 * good, 2 * n) + 1e-12 >= wilsonLower (good, n)
    -- 「1件中1件は 100% ではない。30件中24件のほうが上」
    it "does not believe 1 out of 1" $ do
      showShare (wilsonLower (1, 1))   `shouldBe` "21%"
      showShare (wilsonLower (24, 30)) `shouldBe` "63%"
      wilsonLower (0, 0) `shouldBe` 0
      titles (rankCards (wilsonLower . liked 80) myShelf)
        `shouldBe` [ "迷宮図書館の写本係", "王都の路地裏探偵"
                   , "はじめてのドラゴン学", "図書室の司書に転生した件"
                   , "最後のドラゴン" ]

  -- 「項目ごとの評価と、見せ方」
  describe "ratings on entries, and how they look" $ do
    -- 「目次にない話には評価を付けられない。付けた評価は全部集まる」
    it "rates entries in the table, and gathers every rating" $ do
      rateEntry (EntryId 99) (surely (score 80)) librarianCard
        `shouldBe` Left (NoSuchEntry (EntryId 99))
      map scoreValue (ratingsOf dragonCard)    `shouldBe` [90]
      map scoreValue (ratingsOf librarianCard) `shouldBe` [100]
      length (ratingsOf copyistCard)          `shouldBe` 30
      ratingsOf (newCard dungeonGame)         `shouldBe` []
    -- 「目盛りに寄せた点は、元の点と同じに見える」
    prop "snaps a score to what the style shows" $
      forAll (elements allStyles) $ \style ->
      forAll (choose (0, 100)) $ \n ->
        let s = surely (score n)
        in  render style (snap style s) === render style s
              .&&. snap style s `elem` marks style
    -- 「ヒストグラムは、評価を1件残らず数える」
    prop "counts every rating in the histogram" $
      forAll (elements allStyles) $ \style -> \xs ->
        let rows = histogram 10 style (scoresOf xs)
        in  sum (map (read . T.unpack . last . T.words) rows)
              === length xs
    -- 「平均の見せ方と、写本係のヒストグラム」
    it "shows averages and draws a histogram" $ do
      map (`showAverage` (3182 / 35)) allStyles
        `shouldBe` ["90.9点", "★4.5", "9.1/10", "9.1/10"]
      showAverage FiveStars 87 `shouldBe` "★4.4"
      histogram 10 FiveStars (ratingsOf copyistCard)
        `shouldBe` [ "★5   ▓▓▓▓▓▓▓▓▓▓ 18"
                   , "★4.5 ▓▓▓▓       6"
                   , "★4              0"
                   , "★3.5 ▓▓▓▓       6" ]
  -- ANCHOR_END: rankingSpec

  -- 「トレーニングの解答」
  describe "answers" $ do
    -- 「読みかけを半分、最初の未読、色つきのバー」
    it "counts halves, finds the first unread, and colors the bar" $ do
      let finished = finishedIn dragonLog
      progressWithHalf (Set.fromList (map EntryId [1 .. 4]))
                       (Set.singleton (EntryId 5)) dragonToc
        `shouldBe` pages 36 78
      fmap heading (firstUnread finished dragonToc)
        `shouldBe` Just "第2部 ドラゴンと話す"
      firstUnread (Set.fromList (map EntryId [1 .. 8])) dragonToc
        `shouldBe` Nothing
      colorBar 10 (progressOf finished dragonToc)
        `shouldBe` "\ESC[33m▓▓▓▓▓▓░░░░  62%\ESC[0m"
    -- 「成績で見せて読み戻すと、同じ成績。モデルは変えずに足せた」
    it "adds grades without touching the model" $ do
      fmap renderGrade (score 73) `shouldBe` Right "C"
      fmap renderGrade (score 59) `shouldBe` Right "F"
      [ fmap renderGrade (parseGrade g) | g <- ["A", "B", "C", "D", "F"] ]
        `shouldBe` map Right ["A", "B", "C", "D", "F"]
      parseGrade "E" `shouldBe` Left (BadRating "E")
    -- 「何件ぶんで1位が変わる。後の話ほど重い平均。種類ごとのランキング」
    it "moves the top, weighs later episodes, and ranks by kind" $ do
      map (`leaderFor` myShelf) [0, 1, 2, 5, 50]
        `shouldBe` map Just [ "図書室の司書に転生した件"
                            , "図書室の司書に転生した件"
                            , "迷宮図書館の写本係"
                            , "迷宮図書館の写本係"
                            , "迷宮図書館の写本係" ]
      leaderFor 5 [] `shouldBe` Nothing
      recentForm detectiveCard `shouldBe` Just 81
      recentForm filmCard      `shouldBe` Nothing
      laterHeavier (scoresOf [60, 90]) `shouldBe` Just 80
      titles (rankKind 5 Serials myShelf)
        `shouldBe` ["図書室の司書に転生した件", "迷宮図書館の写本係"]
      map fst (rankKind 5 Serials myShelf) `shouldBe` [290 / 3, 3240 / 35]
      rankKind 5 Games myShelf `shouldBe` []

  -- ANCHOR: shelfSpec
  customSpec
  -- 「本を取り込む」の章のテスト
  importSpec
  -- 「中身を探す」の章のテスト
  searchSpec
  -- 「窓口と記録」の章のテスト
  shelfSpec
  -- 「画面に依らない API」の章のテスト
  apiSpec
  htmlSpec
  tuiSpec
  mcpSpec
  reviewSpec
  editSpec
  -- ANCHOR_END: shelfSpec
-- ANCHOR_END: spec
