-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
-- 🧪 The reading circle: where everyone is, notes seen with another's
--    eyes, and up to where we can all talk.
-- 読書会のテスト：みんながどこまで、ほかの人の目で見た感想、どこまでなら
-- みんなで話せるか
module Main (main) where

import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Tsundoku.Card (Card (..))
import Tsundoku.Examples (dragonCard, finishedDragon, nov)
import Tsundoku.Progress (finishedIn)
import Tsundoku.Review (Seen (..))
import Tsundoku.Types

import Review.BookClub
-- ANCHOR_END: imports

-- ANCHOR: helpers
-- 📝 The bodies I may read, and Nothing for each hidden note.
-- 読んでよい本文と、隠した感想ごとの Nothing
bodies :: [Seen] -> [Maybe Text]
bodies = map see
  where
    see (Shown n)  = Just n.body
    see (Hidden _) = Nothing

lambdaCard, tsCard :: Card
lambdaCard = case sampleClub of
  m : _ -> m.card
  []    -> finishedDragon
tsCard = dragonCard

-- 🎲 A reader who has read some of the dragon book's entries, in any order.
-- ドラゴン学の項目をいくつか（どんな順でも）読んだ人
readerWith :: [Int] -> Card
readerWith ns = dragonCard
  { readLog = [ LogLine (nov 1) (EntryId n) | n <- ns ]
  , reading = Reading }
-- ANCHOR_END: helpers

main :: IO ()
main = hspec $ do
  -- ANCHOR: step1
  describe "where everyone is" $
    -- 「みんなのバー」
    it "draws a bar for each member" $
      progressLines sampleClub `shouldBe`
        [ "ラムダくん  ▓▓▓▓▓▓▓▓▓▓ 100%  78/78ページ"
        , "TS くん     ▓▓▓▓▓▓░░░░  62%  48/78ページ" ]
  -- ANCHOR_END: step1

  -- ANCHOR: step2
  describe "notes, with another's eyes" $ do
    -- 「TS くんには、最後の章のネタバレは見えない」
    it "hides from TS-kun the spoiler on the last chapter" $
      bodies (seenBy tsCard lambdaCard) `shouldBe`
        [ Just "絵が多くて読みやすい", Just "火を吐かない種もいるとは！", Nothing ]
    -- 「ラムダくんには、全部見える」
    it "shows ラムダくん everything" $
      bodies (seenBy lambdaCard lambdaCard) `shouldBe`
        [ Just "絵が多くて読みやすい", Just "火を吐かない種もいるとは！"
        , Just "最後は、竜と和解する" ]
    -- 「ラムダくんが、まだ話してはいけない章」
    it "says what ラムダくん should not talk about yet" $
      ahead lambdaCard tsCard `shouldBe`
        ["第2部 ドラゴンと話す", "第3章 あいさつの作法", "第4章 怒らせたときは"]
  -- ANCHOR_END: step2

  -- ANCHOR: step3
  describe "no spoiler leaks" $
    -- 「どんなに飛ばし読みしても、見えるネタバレは、読んだ項目のものだけ」
    prop "shows a spoiler only on an entry the reader has read" $
      forAll (sublistOf [1 .. 8]) $ \ns ->
        let reader = readerWith ns
            read'  = finishedIn reader.readLog
        in  and [ case n.target of
                    OnEntry e -> e `Set.member` read'
                    OnWork    -> False
                | Shown n <- seenBy reader lambdaCard, n.spoiler ]
  -- ANCHOR_END: step3

  -- ANCHOR: step4
  describe "the meeting point" $ do
    -- 「みんなが読んだのは、第2章まで」
    it "is where everyone has read up to, with no gap" $
      meetingPoint sampleClub `shouldBe` Just "第2章 すみかと季節"
    -- 「飛ばし読みは、すき間の前まで」
    it "stops before a gap" $
      meetingPoint [Member "a" (readerWith [1, 2, 4]), Member "b" lambdaCard]
        `shouldBe` Just "第1章 見分け方"
    -- 「だれかが最初の項目も読んでいなければ、なし」
    it "is nothing when someone has not begun" $
      meetingPoint [Member "a" (readerWith []), Member "b" lambdaCard]
        `shouldBe` Nothing
    -- 「最初の項目を読んでいれば、かならず集まれる」
    prop "has a point exactly when the first entry is read" $
      forAll (sublistOf [1 .. 8]) $ \ns ->
        let club = [Member "a" (readerWith ns), Member "b" lambdaCard]
        in  isJust (meetingPoint club) === (1 `elem` ns)
  -- ANCHOR_END: step4
-- ANCHOR_END: all
