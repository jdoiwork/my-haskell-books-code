-- ANCHOR: all
-- | Ticket numbers for visitors, settings that can be swapped, and a
--   fake clock for tests: the everyday uses of IORef.
-- 訪問者の通し番号、差し替えられる設定、テスト用の偽の時計：
-- IORef のふだんの使い方
module Tickets
  ( newTicketBox
  , takeTicket
  , ticketRush
  , Settings (..)
  , defaultSettings
  , Clock (..)
  , systemClock
  , fakeClock
  , isOpen
  ) where

-- ANCHOR: imports
import Control.Concurrent.Async (forConcurrently)
import Data.IORef
import Data.List (sort)
import Data.Time
  (getZonedTime, localTimeOfDay, todHour, zonedTimeToLocalTime)
-- ANCHOR_END: imports

-- ANCHOR: ticket
-- 🎫 A box with the next ticket number in it.
-- 次に渡す番号が入った箱
newTicketBox :: IO (IORef Int)
newTicketBox = newIORef 1

-- 🎟️ Hand out the number in the box, and put the next one in.
-- 箱の番号を渡して、次の番号を入れておく
takeTicket :: IORef Int -> IO Int
takeTicket box = atomicModifyIORef' box (\n -> (n + 1, n))
-- ANCHOR_END: ticket

-- ANCHOR: rush
-- 🏃 100 visitors at once, 10 tickets each. All of 1 to 1000, once each?
-- 100人が同時に10枚ずつ。1〜1000 が、1回ずつ配られた？
ticketRush :: IO Bool
ticketRush = do
  box <- newTicketBox
  perVisitor <- forConcurrently [1 .. 100 :: Int] $ \_ ->
    mapM (\_ -> takeTicket box) [1 .. 10 :: Int]
  let numbers = sort (concat perVisitor)
  pure (numbers == [1 .. 1000])
-- ANCHOR_END: rush

-- ANCHOR: settings
-- ⚙️ The guild's opening hours. Read often, changed now and then.
-- ギルドの営業時間。よく読み、ときどき変える
data Settings = Settings
  { openHour  :: Int
  , closeHour :: Int
  }
  deriving (Show)

defaultSettings :: Settings
defaultSettings = Settings { openHour = 9, closeHour = 18 }

-- ⏰ Ask what hour it is.
-- いま何時かを聞く
newtype Clock = Clock { currentHour :: IO Int }

-- 🕰️ The real clock on the wall.
-- 壁に掛かった本物の時計
systemClock :: Clock
systemClock = Clock $ do
  now <- getZonedTime
  pure (todHour (localTimeOfDay (zonedTimeToLocalTime now)))

-- 🧪 A fake clock for tests: the hour is in a box, and the test
--    moves the hands.
-- テスト用の偽の時計：時刻は箱の中。テストが針を動かす
fakeClock :: Int -> IO (Clock, Int -> IO ())
fakeClock start = do
  box <- newIORef start
  pure (Clock (readIORef box), writeIORef box)

-- 🚪 Is the guild open now?
-- いま、ギルドは開いている？
isOpen :: IORef Settings -> Clock -> IO Bool
isOpen settingsBox clock = do
  settings <- readIORef settingsBox
  hour     <- currentHour clock
  pure (openHour settings <= hour && hour < closeHour settings)
-- ANCHOR_END: settings
-- ANCHOR_END: all
