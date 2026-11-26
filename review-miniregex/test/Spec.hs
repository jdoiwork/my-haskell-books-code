-- ANCHOR: all
-- ANCHOR: imports
-- | The tests of the old ledger: examples, then properties with QuickCheck.
-- 古い帳簿のテスト：例と、QuickCheck の性質
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
import Text.Regex.Pcre2 qualified as Pcre2

import MiniRegex (source)
import Review.Ledger
-- ANCHOR_END: imports

-- ANCHOR: gen
-- 🎲 A line of an old ledger, made of pieces that look like one.
--    The pieces have digits in ASCII and in full width, and letters in
--    English and in Japanese.
-- 古い帳簿らしい部品をつないだ1行。部品には、ASCII と全角の数字、
-- 英語と日本語の文字がある
genLine :: Gen Text
genLine = T.concat <$> listOf (elements pieces)
  where
    pieces =
      [ "21", "11", "2026", "0", "7", "１", "２０２６"
      , "/", "-", " ", "g", "G", "gold", "old"
      , "Mira", "sold", "for", "herbs", "金貨", "の"
      -- whole dates and halves of them: one at a time, the pieces
      -- above would almost never line up into 21/11/2026
      -- 日付まるごとと、その半分：上の部品を1つずつつなぐだけでは、
      -- 21/11/2026 の形には、まず並ばない
      , "21/11/2026", "21/11/", "/11/2026", "2026-11-21", "21/11/２０２６" ]

newtype Line = Line Text
  deriving (Show)

instance Arbitrary Line where
  arbitrary = Line <$> genLine
  shrink (Line t) = map (Line . T.pack) (shrink (T.unpack t))
-- ANCHOR_END: gen

-- ANCHOR: pcre2
-- 🐪 The same fix with the real PCRE2: the same patterns, the same
--    templates. gsub replaces every match, like replaceAllTemplate.
-- 本物の PCRE2 で、同じように直す。パターンもテンプレートも同じ。
-- gsub は、replaceAllTemplate と同じく、一致を全部置き換える
fixLinePcre2 :: Text -> Text
fixLinePcre2 =
  Pcre2.gsub (source goldRegex) goldPlan
    . Pcre2.gsub (source oneDate) datePlan

fixLineAsciiPcre2 :: Text -> Text
fixLineAsciiPcre2 =
  Pcre2.gsub (source asciiGold) goldPlan
    . Pcre2.gsub (source asciiDate) datePlan
-- ANCHOR_END: pcre2

-- ANCHOR: main
main :: IO ()
main = hspec $ do
  -- ANCHOR_END: main
  -- ANCHOR: examples
  describe "the old ledger" $ do
    -- 「古い日付を、新しい書き方に」
    it "writes old dates the new way" $
      fixDates "21/11/2026 Mira sold 300g"
        `shouldBe` "2026-11-21 Mira sold 300g"
    -- 「金貨の書き方をそろえて、1行を読む」
    it "reads a fixed line" $
      readEntry (fixLine "21/11/2026 Mira sold 300g for herbs")
        `shouldBe` Just (Entry "2026-11-21" "Mira" 300 (Just "herbs"))
    -- 「人ごとに、金貨を足す」
    it "adds up the gold of each person" $
      totals oldPage
        `shouldBe` Map.fromList [("Mira", 420), ("Ruby", 25)]
    -- 「下書きでは、120 gold の行が消える」
    it "loses a line with the draft" $
      totalsWith fixLineDraft oldPage
        `shouldBe` Map.fromList [("Mira", 300), ("Ruby", 25)]
    -- 「Int に収まる2行を足しても、あふれない」
    it "adds up past the end of Int" $
      totals [ "2026-11-21 Mira sold 9223372036854775807 gold"
             , "2026-11-21 Mira sold 1 gold" ]
        `shouldBe` Map.fromList [("Mira", 9223372036854775808)]
    -- 「帳簿の数は ASCII：全角の数字の行は、読まない」
    it "reads the numbers in ASCII only" $ do
      totalsAscii ["21/11/２０２６ Mira sold 1g", "２０２６-11-21 Mira sold 1 gold"]
        `shouldBe` Map.empty
      totals ["21/11/２０２６ Mira sold 1g"] `shouldBe` Map.empty
      totalsAscii oldPage `shouldBe` Map.fromList [("Mira", 420), ("Ruby", 25)]
    -- 「売り上げらしいのに読めなかった行は、黙って捨てずに返す」
    it "gives back the sales it could not read" $
      unreadSales [ "21/11/2026 Mira sold 300g for healing herbs"
                  , "21/11/2026 Mira sold 1,200g", "22/11/2026 the cat slept"
                  , "21/11/2026 Ruby sold 25g" ]
        `shouldBe` [ "21/11/2026 Mira sold 300g for healing herbs"
                   , "21/11/2026 Mira sold 1,200g" ]
  -- ANCHOR_END: examples

  -- ANCHOR: twice
  describe "fixing twice" $ do
    -- 「下書きは、2回直すと変わってしまう（QuickCheck が破る）」
    prop "draft: fixing twice is fixing once (QuickCheck breaks it)" $
      expectFailure $ \(Line t) ->
        fixLineDraft (fixLineDraft t) === fixLineDraft t
    -- 「見つかった反例」
    it "the draft writes gold twice" $
      fixLineDraft (fixLineDraft "0g") `shouldBe` "0 goldold"
  -- ANCHOR_END: twice

  -- ANCHOR: fixedtests
  describe "the fixed fix" $ do
    -- 「直した版は、2回直しても1回と同じ」
    prop "fixing twice is fixing once" $ \(Line t) ->
      fixLine (fixLine t) === fixLine t
    -- 「Gold も gold に。gems の g は金貨ではない」
    it "writes Gold out, and leaves gems alone" $ do
      fixLine "22/11/2026 Mira sold 300 Gold"
        `shouldBe` "2026-11-22 Mira sold 300 gold"
      fixLine "Mira has 300 gems" `shouldBe` "Mira has 300 gems"
    -- 「日付の並びの一部は、日付ではない」
    it "leaves a run of dates alone" $ do
      fixLine "21/11/21/11/2026" `shouldBe` "21/11/21/11/2026"
      fixLine "21/11/2026/11" `shouldBe` "21/11/2026/11"
  -- ANCHOR_END: fixedtests

  -- ANCHOR: compare
  describe "the real PCRE2" $ modifyMaxSuccess (const 2000) $ do
    -- 「(?a) なしでは、本物の PCRE2 と答えがちがう（QuickCheck が破る）」
    prop "without (?a): the same answer as PCRE2 (QuickCheck breaks it)" $
      expectFailure $ \(Line t) ->
        fixLine t === fixLinePcre2 t
    -- 「(?a) を付ければ、本物の PCRE2 と同じ答え」
    prop "with (?a), the same answer as PCRE2" $ \(Line t) ->
      fixLineAscii t === fixLineAsciiPcre2 t
  -- ANCHOR_END: compare

  -- ANCHOR: asciitests
  describe "the ASCII way" $ do
    -- 「ASCII の決まりでも、2回直しても1回と同じ。全角の年は直さない」
    prop "the ASCII way: fixing twice is fixing once" $ \(Line t) ->
      fixLineAscii (fixLineAscii t) === fixLineAscii t
    it "leaves a full-width year alone" $ do
      fixLineAscii "21/11/２０２６ Mira sold 1g"
        `shouldBe` "21/11/２０２６ Mira sold 1 gold"
      fixLineAscii "21/11/21/11/2026" `shouldBe` "21/11/21/11/2026"
  -- ANCHOR_END: asciitests
-- ANCHOR_END: all
