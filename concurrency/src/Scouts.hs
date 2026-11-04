-- ANCHOR: all
-- 🔭 Scouts that take a while to come back: waiting for many at once.
-- 戻るのに時間がかかる偵察隊：いくつもを同時に待つ
module Scouts (scout, scoutsOneByOne, scoutsAtOnce, scoutWithin) where

import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (mapConcurrently, race)

-- ANCHOR: scout
-- 🔭 A scout goes out, and comes back after some milliseconds.
-- 偵察に出て、何ミリ秒かたってから戻ってくる
scout :: (String, Int) -> IO String
scout (place, millis) = do
  threadDelay (millis * 1000)
  pure (place ++ ": " ++ show (millis `div` 100) ++ " goblins")
-- ANCHOR_END: scout

-- ANCHOR: many
-- 🐢 One after another: the times add up.
-- 1人ずつ順に：時間が足し算になる
scoutsOneByOne :: [(String, Int)] -> IO [String]
scoutsOneByOne places = mapM scout places

-- 🐇 All at once: we wait only as long as the slowest one.
-- 全員いっせいに：いちばん遅い人の分だけ待てばよい
scoutsAtOnce :: [(String, Int)] -> IO [String]
scoutsAtOnce places = mapConcurrently scout places
-- ANCHOR_END: many

-- ANCHOR: race
-- ⏰ Give up if the scout is not back within the time limit.
-- 時間内に戻らなければ、あきらめる
scoutWithin :: Int -> (String, Int) -> IO (Either () String)
scoutWithin limit place = race (threadDelay (limit * 1000)) (scout place)
-- ANCHOR_END: race
-- ANCHOR_END: all
