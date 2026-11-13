-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}
-- 🪧 Post a quest, and show it in the wall clock of every branch.
--    cabal run guild-board                          (now)
--    cabal run guild-board -- 2026-11-12T21:00:00Z  (a fixed moment)
-- 依頼を貼り、どの支部の壁の時計でも見せる
-- ANCHOR: module
module Main (main) where

import Data.Time
  ( defaultTimeLocale
  , formatTime
  , getCurrentTimeZone
  )
import Data.Time.Format.ISO8601 (iso8601ParseM)
import System.Environment (getArgs)
import System.Exit (die)

import Board (Quest (..), noticeLine)
import Branches (Branch, wallClockAt)
import Clock (Clock, postQuest, stoppedAt, systemClock)
import Offsets (toWire)
-- ANCHOR_END: module

-- ANCHOR: main
main :: IO ()
main = do
  args <- getArgs
  clock <- clockFrom args
  -- The time zone of this computer (TZ, or the system setting).
  -- このパソコンの時間帯（TZ か、システムの設定）
  here <- getCurrentTimeZone
  quest <- postQuest clock here "Slay the goblins"
  putStrLn (noticeLine here quest)
  putStrLn ("  UTC      : " <> toWire quest.postedAt)
  mapM_ (putStrLn . branchLine quest) [minBound .. maxBound]

-- 🕰️ The real clock, or a clock stopped at the moment in the arguments.
-- 本物の時計か、引数の日時で止まった時計
clockFrom :: [String] -> IO Clock
clockFrom []     = pure systemClock
clockFrom [text] =
  case iso8601ParseM text of
    Just moment -> pure (stoppedAt moment)
    -- 「ISO 8601 の日時ではありません」
    Nothing     -> die ("not an ISO 8601 moment: " <> text)
-- 「使い方：guild-board [日時]」
clockFrom _      = die "usage: guild-board [2026-11-12T21:00:00Z]"

-- 🏰 "  NewYork  : 2026-11-12 16:00 EST"
branchLine :: Quest -> Branch -> String
branchLine quest branch =
  let wall = wallClockAt branch quest.postedAt
      name = take 9 (show branch <> repeat ' ')
      shown = formatTime defaultTimeLocale "%Y-%m-%d %H:%M %Z" wall
  in "  " <> name <> ": " <> shown
-- ANCHOR_END: main
-- ANCHOR_END: all
