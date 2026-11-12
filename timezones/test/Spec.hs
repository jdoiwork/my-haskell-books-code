-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}
-- 🧪 Tests for the clock of the quest board, with stopped clocks.
-- 掲示板の時計のテスト。止まった時計で
-- ANCHOR: module
module Main (main) where

import Data.Time
import Test.Hspec

import Board (Quest (..), postCareless, postIn)
import Branches (Branch (..), Reading (..), readWallClock, sameTimeNextDay)
import Clock (postQuest, stoppedAt)
import Offsets (fromWire, fromWireAny)
import Wire (fromJsMillis, toJsMillis)
import ZoneAnswers
import Zones (jst)
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

  -- 「境目」
  describe "the borders" $ do
    -- 「Z のない ISO 8601 は、UTCTime としては読まない。ずれつきでも読む」
    it "reads a moment with an offset only when asked to" $ do
      let tokyo = "2026-11-13T06:00:00+09:00"
      fromWire tokyo `shouldBe` Nothing
      fromWireAny tokyo `shouldBe` Just sixInTokyo
    -- 「ミリ秒の往復では、ミリ秒より細かい所が消える」
    it "drops the part below a millisecond on the way to JavaScript" $ do
      let moment = addUTCTime 0.0012345 sixInTokyo
      toJsMillis moment `shouldBe` 1794517200001
      fromJsMillis (toJsMillis moment) `shouldBe` addUTCTime 0.001 sixInTokyo

  -- 「トレーニングの解答」
  describe "the answers" $ do
    -- 「ステップ1：支部の壁の時計で1行に」
    it "step 1: shows a quest in a branch's wall clock" $ do
      let quest = postIn jst "Slay the goblins" sixInTokyo
      noticeIn NewYork quest
        `shouldBe` "Slay the goblins (posted 2026-11-12 16:00 EST)"
      noticeIn Tokyo quest
        `shouldBe` "Slay the goblins (posted 2026-11-13 06:00 JST)"
    -- 「ステップ2：あいさつは時計と時間帯で決まる」
    it "step 2: greets by the clock and the time zone" $ do
      greeting (stoppedAt sixInTokyo) jst `shouldReturn` "Good morning"
      greeting (stoppedAt sixInTokyo) utc `shouldReturn` "Good evening"
    -- 「ステップ3：夏時間が終わると、会議は UTC で1時間遅くなる」
    it "step 3: the meeting moves by one hour in UTC" $
      map (todHour . timeToTimeOfDay . utctDayTime)
          (dailyMeetings NewYork (fromGregorian 2026 10 31) 3 (TimeOfDay 9 0 0))
        `shouldBe` [13, 14, 14]
-- ANCHOR_END: all
