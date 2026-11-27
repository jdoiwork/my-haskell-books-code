-- ANCHOR: all
-- 🧪 Tests for the calendar, the lengths of time, and the formats.
-- 暦の計算、時間の長さ、書式のテスト
-- ANCHOR: module
module Main (main) where

import Data.Time
import Test.Hspec
import Test.QuickCheck

import Calendar (onBoardDay, payDayClip, payDayRollOver, payDaysDrift)
import DateAnswers
import Formats (dueDayFrom, fromLondon, fromNewYork, fromWire, toWire)
import Spans (servedFor, showLeft, termEnds)
-- ANCHOR_END: module

-- ANCHOR: moments
-- 🎲 Moments from the year 1 to 9999, down to the picosecond.
-- 1年から 9999 年までの日時。ピコ秒まで
moments :: Gen UTCTime
moments = do
  -- Days are counted from 1858-11-17 (the Modified Julian Day).
  -- Day は 1858-11-17 からの日数（修正ユリウス日）で作れる
  n <- choose ( toModifiedJulianDay (fromGregorian 1 1 1)
              , toModifiedJulianDay (fromGregorian 9999 12 31) )
  pico <- choose (0, 86400 * 10 ^ (12 :: Int) - 1)
  pure (UTCTime (ModifiedJulianDay n) (picosecondsToDiffTime pico))
-- ANCHOR_END: moments

-- ANCHOR: day
-- 📅 A day in 2027, for short.
-- 2027年の日付を短く書く
day :: Int -> Int -> Day
day m d = fromGregorian 2027 m d
-- ANCHOR_END: day

-- ANCHOR: spec
main :: IO ()
main = hspec $ do
  -- 「月を足す」
  describe "adding months" $ do
    -- 「Clip は月末に切り詰め、RollOver は次の月へはみ出す」
    it "clips or rolls over the 31st of January" $ do
      payDayClip 1 `shouldBe` day 2 28
      payDayRollOver 1 `shouldBe` day 3 3
    -- 「くり返し足すと、日がずれる」
    it "drifts when added again and again" $
      payDaysDrift !! 2 `shouldBe` day 3 28
    -- 「土日の締め切りは、月曜日にずらす」
    it "moves a due day on a weekend to Monday" $
      onBoardDay (fromGregorian 2026 11 21)
        `shouldBe` fromGregorian 2026 11 23

  -- 「書式」
  describe "formats" $ do
    -- ANCHOR: roundtrip
    -- 「ISO 8601 の文字は、もとの日時に戻る」
    it "reads back what ISO 8601 wrote" $
      property $ forAll moments $ \moment ->
        fromWire (toWire moment) === Just moment
    -- ANCHOR_END: roundtrip
  -- ANCHOR_END: spec
    -- 「同じ 11/12/2026 が、支部によって別の日になる」
    it "reads 11/12/2026 as two different days" $ do
      fromNewYork "11/12/2026" `shouldBe` Just (fromGregorian 2026 11 12)
      fromLondon "11/12/2026" `shouldBe` Just (fromGregorian 2026 12 11)
    -- 「2月28日は読めて、2月30日は読めない」
    it "reads 02/28 but not 02/30" $ do
      fromNewYork "02/28/2026" `shouldBe` Just (fromGregorian 2026 2 28)
      fromNewYork "02/30/2026" `shouldBe` Nothing
    -- 「読めても、意味の通らない締め切りは断る」
    it "refuses a due day that can be read but makes no sense" $ do
      let today = fromGregorian 2026 11 13
      dueDayFrom today "2026-11-20"
        `shouldBe` Right (fromGregorian 2026 11 20)
      dueDayFrom today "-0001-01-01"
        `shouldBe` Left "already past: -0001-01-01"
      dueDayFrom today "2026-02-30"
        `shouldBe` Left "not a day: \"2026-02-30\""
    -- 「1年後は、暦の上の同じ日まで（2月29日をまたいでも）」
    it "takes one year on the calendar, over a 29th of February" $ do
      let today = fromGregorian 2027 3 1
      dueDayFrom today "2028-03-01"
        `shouldBe` Right (fromGregorian 2028 3 1)
      dueDayFrom today "2028-03-02"
        `shouldBe` Left "too far away: 2028-03-02"
    -- 「9999 年の終わりまで書いて読める」
    it "writes and reads the last moment of 9999" $ do
      let lastDay = UTCTime (fromGregorian 9999 12 31) 86399
      toWire lastDay `shouldBe` "9999-12-31T23:59:59Z"
      fromWire (toWire lastDay) `shouldBe` Just lastDay

  -- 「時間の長さ」
  describe "lengths of time" $ do
    -- 「残り時間は、分と秒を 0 で埋めて見せる」
    it "shows the time left with padded minutes and seconds" $
      showLeft (26 * 3600 + 5 * 60 + 7) `shouldBe` "26:05:07"
    -- 「期限を過ぎたら、マイナスは頭に1つだけ」
    it "shows one minus sign past the limit" $ do
      showLeft (-61) `shouldBe` "-0:01:01"
      showLeft (-1) `shouldBe` "-0:00:01"
    -- 「1月31日から1か月と10日」
    it "adds one month and ten days to the 31st of January" $
      termEnds (day 1 31) `shouldBe` day 3 10
    -- 「何か月と何日たったか」
    it "tells how long a member has served" $
      servedFor (day 3 15) (day 1 31) `shouldBe` CalendarDiffDays 1 15

  -- 「トレーニングの解答」
  describe "the answers" $ do
    -- 「ステップ1：締め切りまでの日数と曜日」
    it "step 1: tells the days left and the day of the week" $ do
      let today = fromGregorian 2026 11 13
      dueNotice today (fromGregorian 2026 11 20)
        `shouldBe` "あと7日（金曜日）"
      dueNotice today today `shouldBe` "今日まで（金曜日）"
      dueNotice today (fromGregorian 2026 11 10)
        `shouldBe` "3日過ぎた（火曜日）"
    -- 「ステップ2：支払日と月末」
    it "step 2: lists pay days and the ends of months" $ do
      payDays 4 `shouldBe` [day 1 31, day 2 28, day 3 31, day 4 30]
      monthEnds (dayPeriod (fromGregorian 2028 1 1)) 3 `shouldBe`
        map (uncurry (fromGregorian 2028)) [(1, 31), (2, 29), (3, 31)]
    -- 「ステップ3：和風月名」
    it "step 3: shows the old names of the months" $
      showWafu (fromGregorian 2026 11 13) `shouldBe` "霜月13日（金）"
    -- 「ステップ4：残り時間を日・時間・分で」
    it "step 4: tells the time left in days, hours, and minutes" $ do
      remaining (26 * 3600 + 5 * 60) `shouldBe` "あと1日2時間5分"
      remaining 30 `shouldBe` "あと0日0時間1分"
      remaining 0 `shouldBe` "締め切りを過ぎた"
    -- 「ステップ4：1分ちょうどと、その前後（ピコ秒の端数も切り上げる）」
    it "step 4: rounds up even a picosecond" $ do
      remaining 60 `shouldBe` "あと0日0時間1分"
      remaining (60 + 1e-12) `shouldBe` "あと0日0時間2分"
      remaining 1e-12 `shouldBe` "あと0日0時間1分"
-- ANCHOR_END: all
