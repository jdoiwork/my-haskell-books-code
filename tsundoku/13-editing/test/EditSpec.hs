-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes         #-}
-- 🧪 Fixing a work by hand: the core (numbers kept, what hung on a line
--    that is gone), the shape of a table, and the API's route.
-- 作品を手で直す：核（番号を保つ、消えた行にぶら下がっていたもの）、
-- 目次の形、API の道
module EditSpec (editSpec) where

-- JSON, dictionaries, and trees
-- JSON、辞書、木
import Data.Aeson (decode)
import Data.List (subsequences)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Tree (Tree (..), flatten)

-- the tests: hspec, QuickCheck, and hspec-wai
-- テスト：hspec、QuickCheck、hspec-wai
import Network.HTTP.Types (methodPost, methodPut)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Wai hiding (pending)
import Test.Hspec.Wai.JSON (json)
import Test.QuickCheck

import Tsundoku.Api.Json
import Tsundoku.Shelf (Shelf (..), WorkWindow (..))
import Tsundoku.Shelf.Memory (newMemoryShelf)
import Tsundoku.Card
import Tsundoku.Examples
import Tsundoku.Rating (score)
import Tsundoku.TocCheck
import Tsundoku.Types

import ApiSpec (freshApp, refusedWith, send)
import ShelfAnswers.ByHeading
import ShelfAnswers.EditToc
-- ANCHOR_END: imports

editSpec :: Spec
editSpec = do
  coreSpec
  shapeSpec
  routeSpec
  answersSpec

-- ANCHOR: coreSpec
-- ✏️ The core: what stays, what moves, what goes.
-- 核：残るもの、移るもの、消えるもの
coreSpec :: Spec
coreSpec = describe "fixing a work by hand" $ do
  let fixed = surely (revise (EntryId 9) dragonFix dragonCard)

  -- 「残る行は番号を保ち、新しい行は from からの番号」
  it "keeps the numbers that stay, and numbers the new line" $
    entryIds (contents (work fixed))
      `shouldBe` map EntryId [1, 2, 3, 5, 6, 7, 8, 9]

  -- 「消えた行への感想は、作品全体へ移る。言葉とネタバレの印はそのまま」
  it "moves a note on a line that is gone to the whole work" $
    [ (target n, body n, spoiler n) | n <- notes fixed ] `shouldBe`
      [ (OnWork, "絵が多くて読みやすい", False)
      , (OnWork, "火を吐かない種もいるとは！", True) ]

  -- 「消えた行の読書ログは消え、残る行のログは残る」
  it "drops the log of a line that is gone, and keeps the rest" $
    map readEntry (readLog fixed) `shouldBe` map EntryId [1, 2, 3, 5]

  -- 「消えた行の★は消え、残る行の★は残る」
  it "drops the rating of a line that is gone" $ do
    let rated = surely $ do
          s  <- score 80
          r4 <- rateEntry (EntryId 4) s dragonCard
          rateEntry (EntryId 8) s r4
    fmap (Map.keys . entryRatings) (revise (EntryId 9) dragonFix rated)
      `shouldBe` Right [EntryId 8]

  -- 「何も変えない Revision は、カードを変えない（どの見本でも）」
  it "changes nothing with a revision that changes nothing" $
    sequence_
      [ revise (EntryId 99) (asRevision (work c)) c `shouldBe` Right c
      | c <- [ dragonCard, librarianCard, copyistCard, detectiveCard
             , filmCard ] ]

  -- 「どの行を消しても、感想は1つも消えず、どれもある所を指す」
  prop "never loses a note, whichever lines are gone" $
    forAll (elements (subsequences [1 .. 8])) $ \gone ->
      let r      = dropLines gone (asRevision (work dragonCard))
          result = revise (EntryId 9) r dragonCard
          ok c   = length (notes c) == length (notes dragonCard)
                && all (pointsInto c . target) (notes c)
      in  fmap ok result === Right True

  -- 「消すと選べば、残る行と作品全体の感想だけが、そのまま残る」
  prop "keeps just the notes that still have a place, when asked" $
    forAll (elements (subsequences [1 .. 8])) $ \gone ->
      let r      = (dropLines gone (asRevision (work dragonCard)))
                     { notesOnRemoved = DropNotes }
          result = revise (EntryId 9) r dragonCard
          ok c   = notes c == filter (pointsInto c . target)
                                     (notes dragonCard)
      in  fmap ok result === Right True

  -- 「よその番号、2回出る番号は断る」
  it "refuses a number of another work, or one number twice" $ do
    let table ls = dragonFix { newToc = ls }
        line i   = Node (Item (Just (EntryId i)) "x" 1) []
    revise (EntryId 9) (table [line 42]) dragonCard
      `shouldBe` Left (NoSuchEntry (EntryId 42))
    revise (EntryId 9) (table [line 1, line 1]) dragonCard
      `shouldBe` Left (EntryTwice (EntryId 1))
  where
    pointsInto c (OnEntry i) = i `elem` entryIds (contents (work c))
    pointsInto _ OnWork      = True

-- ✂️ Take these lines (and what is under them) out of a revision.
-- Revision から、この番号の行を（その下ごと）取り除く
dropLines :: [Int] -> Revision -> Revision
dropLines gone r = r { newToc = keep (newToc r) }
  where
    keep ts = [ Node i (keep kids)
              | Node i kids <- ts
              , itemId i `notElem` map (Just . EntryId) gone ]
-- ANCHOR_END: coreSpec

-- ANCHOR: shapeSpec
-- 📏 The shape of a table: the same check for the AI and for a person.
-- 目次の形：AI にも人にも、同じ確かめ
shapeSpec :: Spec
shapeSpec = describe "the shape of a table" $ do
  -- 「まずい所を全部、道順つきで」
  it "finds every problem, with the path to it" $
    shapeProblems
      [ Node "第1章" [Node "1.1" [], Node " " [], Node "1.1" []]
      , Node "第1章" [] ]
      `shouldBe`
        [ EmptyHeading ["第1章"]
        , SameHeading [] "第1章"
        , SameHeading ["第1章"] "1.1" ]

  -- 「深さは4段まで」
  it "says a table is too deep below 4 levels" $
    shapeProblems [Node "1" [Node "2" [Node "3" [Node "4" [Node "5" []]]]]]
      `shouldBe` [TooDeep ["1", "2", "3", "4", "5"]]

  -- 「新しく足す作品も、同じ門を通る」
  it "is checked for a new work too" $
    checkWork (Work "本" "" (Book (numbered [ Node ("同じ", Pages 1) []
                                            , Node ("同じ", Pages 2) [] ])))
      `shouldBe` Left (BadToc [SameHeading [] "同じ"])

  -- 「どの見本の目次も、形はよい」
  it "finds nothing wrong with the examples" $
    [ checkWork w | w <- [ dragonBook, librarianSerial, copyistSerial
                         , detectiveDrama ] ]
      `shouldBe` map Right [dragonBook, librarianSerial, copyistSerial
                           , detectiveDrama]
-- ANCHOR_END: shapeSpec

-- ANCHOR: routeSpec
-- 🌐 PUT /works/{id}: the fixed card, or what is wrong and where.
-- PUT /works/{id}：直したカードか、どこがなぜまずいか
routeSpec :: Spec
routeSpec = describe "the route to fix a work" $ with freshApp $ do
  -- 「直したカードが返り、残る行は番号を保つ」
  it "answers the fixed card, the numbers kept" $ do
    _ <- send methodPost "/works" twoParts
    send methodPut "/works/1" [json|{
      title: "ドラゴン学", creator: "竜の会",
      toc: [ { id: 2, heading: "第2部（前へ）", size: 4, children: [] },
             { id: null, heading: "付録", size: 3, children: [] } ],
      removedNotes: "move"
    }|] `shouldRespondWith` 200
      { matchBody = MatchBody $ \_ body ->
          case decode body :: Maybe CardDetail of
            Just c | [(i.id, i.heading) | i <- c.toc]
                       == [(2, "第2部（前へ）"), (3, "付録")]
                   , c.work.title == "ドラゴン学" -> Nothing
            _ -> Just ("not the fixed card: " <> show body)
      }

  -- 「"move" なら、消える行の感想は作品全体へ移る」
  it "moves the note on a line that is gone, with \"move\"" $ do
    _ <- send methodPost "/works" twoParts
    _ <- send methodPost "/works/1/notes" noteOnFirst
    send methodPut "/works/1" (cutFirst "move")
      `shouldRespondWith` notesOn [Nothing]

  -- 「"drop" なら、消える行の感想も一緒に消える」
  it "drops the note on a line that is gone, with \"drop\"" $ do
    _ <- send methodPost "/works" twoParts
    _ <- send methodPost "/works/1/notes" noteOnFirst
    send methodPut "/works/1" (cutFirst "drop")
      `shouldRespondWith` notesOn []

  -- 「形のまずい目次は 422 で、道順つきの言葉を添える」
  it "refuses a bad shape with 422 and words with the path" $ do
    _ <- send methodPost "/works" twoParts
    send methodPut "/works/1" [json|{
      title: "ドラゴン学", creator: "",
      toc: [ { id: 1, heading: "第1部", size: 2, children: [
               { id: null, heading: "", size: 1, children: [] } ] } ],
      removedNotes: "move"
    }|] `shouldRespondWith` 422
      { matchBody = MatchBody $ \_ body ->
          case decode body :: Maybe Problem of
            Just p | p.mistake == "bad-toc"
                   , p.messages == [ "「第1部」の下に、見出しが空の項目が"
                                     <> "あります。見出しを入れてください" ]
                   -> Nothing
            _ -> Just ("not the words for an empty heading: " <> show body)
      }

  -- 「2回出る番号は 422」
  it "refuses one number twice with 422" $ do
    _ <- send methodPost "/works" twoParts
    send methodPut "/works/1" [json|{
      title: "ドラゴン学", creator: "",
      toc: [ { id: 1, heading: "あ", size: 2, children: [] },
             { id: 1, heading: "い", size: 2, children: [] } ],
      removedNotes: "move"
    }|] `shouldRespondWith` refusedWith 422 "entry-twice"

  -- 「新しく足す作品も、形がまずければ 422」
  it "refuses a new work with a bad shape too" $
    send methodPost "/works" [json|{
      title: "本", creator: "", kind: "book",
      toc: [ { heading: "同じ", size: 1, children: [] },
             { heading: "同じ", size: 1, children: [] } ]
    }|] `shouldRespondWith` refusedWith 422 "bad-toc"
  where
    twoParts = [json|{
      title: "はじめてのドラゴン学", creator: "ギルド図書室", kind: "book",
      toc: [ { heading: "第1部", size: 2, children: [] },
             { heading: "第2部", size: 4, children: [] } ]
    }|]
    noteOnFirst = [json|{entry: 1, body: "第1部の感想", spoiler: false}|]
    -- The first part is gone: only the second stays.
    -- 第1部が消える：第2部だけが残る
    cutFirst :: String -> LBS.ByteString
    cutFirst choice = [json|{
      title: "ドラゴン学", creator: "",
      toc: [ { id: 2, heading: "第2部", size: 4, children: [] } ],
      removedNotes: #{choice}
    }|]
    notesOn entries = 200
      { matchBody = MatchBody $ \_ body ->
          case decode body :: Maybe CardDetail of
            Just c | [n.entry | n <- c.notes] == entries -> Nothing
            _ -> Just ("not the notes expected: " <> show body)
      }
-- ANCHOR_END: routeSpec

-- ANCHOR: answersSpec
-- 🏋️ The training: fixing a work with htmx, as lines with "#numbers".
-- トレーニング：htmx で作品を直す。"#番号" つきの行で
answersSpec :: Spec
answersSpec = describe "the answers" $ do
  let lines' = tocText (detailOf (WorkId 1, dragonCard)).toc

  -- 「行に書き出して読み戻すと、何も変えない Revision になる」
  it "reads its own lines back as a revision that changes nothing" $
    readEdited lines' `shouldBe` Right (newToc (asRevision dragonBook))

  -- 「# のない行は新しい行、読めない行は何行目かつきで」
  it "reads a line with no # as new, and says the lines it cannot read" $ do
    readEdited "#1 第1部 2\n  付録 3\n"
      `shouldBe` Right [ Node (Item (Just (EntryId 1)) "第1部" 2)
                              [Node (Item Nothing "付録" 3) []] ]
    readEdited "#1 第1部 2\n     深すぎ\n"
      `shouldBe` Left [ "2行目「深すぎ」：字下げは、空白2つずつにしてください" ]

  -- 「見出しで突き合わせると、名前を直した行は番号をなくし、感想が移る」
  it "loses the number of a renamed line when matching by heading" $ do
    let typed = [ Node ("第1部 ドラゴンを知る", 2)
                    [ Node ("第1章 見分け方", 4)
                        [ Node ("1.1 うろこの色", 8) []
                        , Node ("1.2 火を吐く竜", 10) [] ] ] ]
        items = byHeading (tocOf dragonBook) typed
    map itemId (concatMap flatten items)
      `shouldBe` [Just (EntryId 1), Just (EntryId 2), Just (EntryId 3), Nothing]
    fmap (map target . notes)
         (revise (EntryId 9) (Revision "竜" "" items MoveToWork) dragonCard)
      `shouldBe` Right [OnWork, OnWork]

  -- 「htmx の PUT で、窓口を通して直す。だめなら 422 で理由」
  describe "with htmx" $ with app $ do
    it "fixes the work through the window" $
      request methodPut "/works/1/edit" form
        "title=%E7%AB%9C&creator=&toc=%231+%E7%AC%AC1%E9%83%A8+2"
        `shouldRespondWith` 200
    it "refuses a heading twice with 422, and says where" $
      request methodPut "/works/1/edit" form
        "title=%E7%AB%9C&creator=&toc=%231+A+2%0AA+3"
        `shouldRespondWith` "" { matchStatus = 422, matchBody =
          bodyContains "目次のいちばん上に「A」が2つあります" }
  where
    form = [("Content-Type", "application/x-www-form-urlencoded")]
    app  = do
      shelf <- newMemoryShelf
      _     <- shelf.works.addWork dragonBook
      pure (editApp shelf)
    tocOf w = case contents w of
      Book toc -> toc
      _        -> []
    bodyContains wanted = MatchBody $ \_ body ->
      if wanted `T.isInfixOf` TE.decodeUtf8 (LBS.toStrict body)
        then Nothing else Just ("no " <> T.unpack wanted)
-- ANCHOR_END: answersSpec
-- ANCHOR_END: all
