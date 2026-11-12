-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}
-- 🧪 Tests for the clock of the quest board, with stopped clocks.
-- 掲示板の時計のテスト。止まった時計で
-- ANCHOR: module
module Main (main) where

import Data.Time
import Test.Hspec
import Test.QuickCheck

import Board (Quest (..), postCareless, postIn)
import Branches (Branch (..), Reading (..), readWallClock, sameTimeNextDay)
import Calendar (payDayClip, payDayRollOver, payDaysDrift)
import Clock (postQuest, stoppedAt)
import DateAnswers
import Formats
  (dueDayFrom, fromLondon, fromNewYork, fromWire, fromWireAny, toWire)
import TimeMap (jst)
-- ANCHOR_END: module

-- ANCHOR: helpers
-- 🧊 6 a.m. on 13 November 2026 in Japan (21:00 the day before in UTC).
-- 日本の 2026年11月13日の朝6時（UTC では前の日の 21 時）
sixInTokyo :: UTCTime
sixInTokyo = UTCTime (fromGregorian 2026 11 12) (21 * 3600)

-- 🕰️ A wall clock time, for short.
-- 壁の時計の時刻を、短く書く
wall :: Integer -> Int -> Int -> Int -> Int -> LocalTime
wall y m d hh mm = LocalTime (fromGregorian y m d) (TimeOfDay hh mm 0)
-- ANCHOR_END: helpers

-- ANCHOR: moments
-- 🎲 Moments from the year 1 to 9999, down to the picosecond.
-- 1年から 9999 年までの瞬間。ピコ秒まで
moments :: Gen UTCTime
moments = do
  -- Days are counted from 1858-11-17 (the Modified Julian Day).
  -- Day は 1858-11-17 からの日数（修正ユリウス日）で作れる
  n <- choose (toModifiedJulianDay (fromGregorian 1 1 1),
               toModifiedJulianDay (fromGregorian 9999 12 31))
  let day = ModifiedJulianDay n
  pico <- choose (0, 86400 * 10 ^ (12 :: Int) - 1)
  pure (UTCTime day (picosecondsToDiffTime pico))
-- ANCHOR_END: moments

-- ANCHOR: spec
main :: IO ()
main = hspec $ do
  -- 「依頼を貼る」
  describe "posting a quest" $ do
    -- 「日本の朝6時は、UTC の日付ではきのう」
    it "is yesterday on the UTC calendar at 6 a.m. in Japan" $
      (postCareless "goblins" sixInTokyo).postedOn
        `shouldBe` fromGregorian 2026 11 12
    -- 「日本の朝6時は、日本の日付では今日」
    it "is today on the calendar of Japan" $ do
      quest <- postQuest (stoppedAt sixInTokyo) jst "goblins"
      quest.postedOn `shouldBe` fromGregorian 2026 11 13
      quest.dueOn `shouldBe` fromGregorian 2026 11 20
    -- 「ニューヨークでは、まだ前の日」
    it "is still the day before in New York" $
      (postIn (hoursToTimeZone (-5)) "goblins" sixInTokyo).postedOn
        `shouldBe` fromGregorian 2026 11 12
  -- ANCHOR_END: spec

  -- 「支部の壁の時計」
  describe "the wall clocks of the branches" $ do
    -- 「春、ニューヨークの時計は 2:30 を飛ばす」
    it "skips 2:30 in New York in spring" $
      readWallClock NewYork (wall 2026 3 8 2 30) `shouldBe` Skipped
    -- 「秋、ニューヨークの時計は 1:30 を2回指す」
    it "shows 1:30 twice in New York in autumn" $
      readWallClock NewYork (wall 2026 11 1 1 30)
        `shouldBe` Twice
          (UTCTime (fromGregorian 2026 11 1) (5.5 * 3600))
          (UTCTime (fromGregorian 2026 11 1) (6.5 * 3600))
    -- 「東京の時計は、どの時刻も1回だけ」
    it "shows every time once in Tokyo" $
      readWallClock Tokyo (wall 2026 3 8 2 30)
        `shouldBe` Once (UTCTime (fromGregorian 2026 3 7) (17.5 * 3600))
    -- 「夏時間の終わりをまたぐと、次の日の同じ時刻は 25 時間後」
    it "has 25 hours until the same time, over the end of summer time" $ do
      let noon = UTCTime (fromGregorian 2026 10 31) (16 * 3600)
      sameTimeNextDay NewYork noon
        `shouldBe` Once (addUTCTime (25 * 3600) noon)

  -- 「月を足す」
  describe "adding months" $ do
    -- 「Clip は月末に切り詰め、RollOver は次の月へはみ出す」
    it "clips or rolls over the 31st of January" $ do
      payDayClip 1 `shouldBe` fromGregorian 2027 2 28
      payDayRollOver 1 `shouldBe` fromGregorian 2027 3 3
    -- 「くり返し足すと、日がずれる」
    it "drifts when added again and again" $
      payDaysDrift !! 2 `shouldBe` fromGregorian 2027 3 28

  -- 「書式」
  describe "formats" $ do
    -- ANCHOR: roundtrip
    -- 「ISO 8601 の文字は、もとの瞬間に戻る」
    it "reads back what ISO 8601 wrote" $
      property $ forAll moments $ \moment ->
        fromWire (toWire moment) === Just moment
    -- ANCHOR_END: roundtrip
    -- 「Z のない ISO 8601 は、UTCTime としては読まない。ずれつきでも読む」
    it "reads a moment with an offset only when asked to" $ do
      let tokyo = "2026-11-13T06:00:00+09:00"
      fromWire tokyo `shouldBe` Nothing
      fromWireAny tokyo `shouldBe` Just (UTCTime (fromGregorian 2026 11 12) 75600)
    -- 「同じ 11/12/2026 が、支部によって別の日になる」
    it "reads 11/12/2026 as two different days" $ do
      fromNewYork "11/12/2026" `shouldBe` Just (fromGregorian 2026 11 12)
      fromLondon "11/12/2026" `shouldBe` Just (fromGregorian 2026 12 11)
    -- 「読めても、意味の通らない締め切りは断る」
    it "refuses a due day that can be read but makes no sense" $ do
      let today = fromGregorian 2026 11 13
      dueDayFrom today "2026-11-20" `shouldBe` Right (fromGregorian 2026 11 20)
      dueDayFrom today "-0001-01-01"
        `shouldBe` Left "already past: -0001-01-01"
      dueDayFrom today "2026-02-30" `shouldBe` Left "not a day: \"2026-02-30\""

  -- 「トレーニングの解答」
  describe "the answers" $ do
    -- 「ステップ1：締め切りまでの日数と曜日」
    it "step 1: tells the days left and the day of the week" $ do
      let today = fromGregorian 2026 11 13
      dueNotice today (fromGregorian 2026 11 20) `shouldBe` "あと7日（金曜日）"
      dueNotice today today `shouldBe` "今日まで（金曜日）"
      dueNotice today (fromGregorian 2026 11 10) `shouldBe` "3日過ぎた（火曜日）"
    -- 「ステップ2：支払日と月末」
    it "step 2: lists pay days and the ends of months" $ do
      payDays 4 `shouldBe`
        map (\(m, d) -> fromGregorian 2027 m d) [(1, 31), (2, 28), (3, 31), (4, 30)]
      monthEnds (dayPeriod (fromGregorian 2028 1 1)) 3 `shouldBe`
        map (\(m, d) -> fromGregorian 2028 m d) [(1, 31), (2, 29), (3, 31)]
    -- 「ステップ3：和風月名」
    it "step 3: shows the old names of the months" $
      showWafu (fromGregorian 2026 11 13) `shouldBe` "霜月13日（金）"
    -- 「ステップ4：あいさつは時計と時間帯で決まる」
    it "step 4: greets by the clock and the time zone" $ do
      greeting (stoppedAt sixInTokyo) jst `shouldReturn` "Good morning"
      greeting (stoppedAt sixInTokyo) utc `shouldReturn` "Good evening"
    -- 「ステップ5：夏時間が終わると、会議は UTC で1時間遅くなる」
    it "step 5: the meeting moves by one hour in UTC" $
      map (todHour . timeToTimeOfDay . utctDayTime)
          (dailyMeetings NewYork (fromGregorian 2026 10 31) 3 (TimeOfDay 9 0 0))
        `shouldBe` [13, 14, 14]
-- ANCHOR_END: all
