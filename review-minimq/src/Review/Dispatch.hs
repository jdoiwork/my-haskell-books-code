{-# LANGUAGE DeriveAnyClass #-}
-- ANCHOR: all
-- | The guild's day of orders: workers that fall over at random, a
--   supervisor that picks them up, and a broker that loses nothing.
-- ギルドの依頼の1日：でたらめに倒れる働き手、それを起こす監督役、
-- そして何もなくさないブローカー
module Review.Dispatch
  ( Order (..)
  , Report (..)
  , Memory (..)
  , runDay
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (link, withAsync)
import Control.Exception (throwIO)
import Control.Monad (forM_, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics (Generic)

import MiniMq.Client (withClient)
import MiniMq.Server (withServer)
import MiniMq.Supervisor
import MiniMq.Typed
import MiniMq.Types

-- ANCHOR: types
-- 📜 An order: its number, and whether it is cursed (no one can finish it).
-- 依頼：番号と、呪われているか（誰にも終わらせられない）
data Order = Order
  { orderNo :: Int
  , cursed  :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 📊 How the day went: how often each order was done, and the numbers
--    of the orders that ended among the dead letters.
-- 1日の結果：それぞれの依頼が何回こなされたかと、
-- 配れなかった手紙の部屋に行きついた依頼の番号
data Report = Report
  { doneCount :: Map Int Int
  , buried    :: [Int]
  }
  deriving (Show, Eq)

-- 🧠 Do the workers remember which orders are done?
-- 働き手は、どの依頼が終わったかを覚えているか
data Memory = Forgetful | Remembering
  deriving (Show, Eq)
-- ANCHOR_END: types

-- ANCHOR: desks
orders :: Desk Order
orders = Desk "orders" Direct json

orderBox :: Mailbox Order
orderBox = Mailbox "orders" plainQueue
  { deadLetterTo = Just "graveyard", deliveryLimit = Just 3 } json

graveyard :: Desk Order
graveyard = Desk "graveyard" Fanout json

graves :: Mailbox Order
graves = Mailbox "graves" plainQueue json
-- ANCHOR_END: desks

-- ANCHOR: runDay
-- 🌅 Run one day. `falls` is the tape of bad luck: each time a worker
--    finishes a job, it reads the next answer, and on True it falls over
--    *after* the job but *before* the ACK — the nastiest moment.
-- 1日を動かす。falls は不運のテープ：働き手は仕事を終えるたびに次の答えを
-- 読み、True なら倒れる。仕事のあと、ACK の前という、いちばん
-- いやなときに
runDay :: Memory -> Int -> [Order] -> [Bool] -> IO Report
runDay memory workers todo falls = withServer $ \port -> do
  withClient port $ \c -> do
    declareDesk c orders
    declareDesk c graveyard
    declareMailbox c orderBox
    declareMailbox c graves
    bindTo c orders "order" orderBox
    bindTo c graveyard "" graves
    forM_ todo (publishTo c orders "order")
  doneRef    <- newIORef Map.empty
  settledRef <- newIORef Set.empty
  buriedRef  <- newIORef []
  tape       <- newIORef (falls <> repeat False)
  -- ANCHOR: job
  let job order = do
        when (cursed order) (throwIO (userError "cursed"))
        done <- readIORef doneRef
        let already = Map.member (orderNo order) done
            tally   = Map.insertWith (+) (orderNo order) 1
        -- Three workers write in this book at once: change it in one step.
        -- 3人が同時に書く帳面なので、1回で書き換える
        when (memory == Forgetful || not already) $
          atomicModifyIORef' doneRef (\counts -> (tally counts, ()))
        unlucky <- atomicModifyIORef' tape nextLuck
        when unlucky (throwIO (userError "tripped before the ACK"))
        -- Past the last chance to fall: only the ACK is left.
        -- もう転ばない。残るのは ACK だけ
        atomicModifyIORef' settledRef
          (\settled -> (Set.insert (orderNo order) settled, ()))
      -- ANCHOR_END: job
      bury order =
        atomicModifyIORef' buriedRef (\dead -> (orderNo order : dead, ()))
      consumeAt box handle = withClient port (\c -> consume c box handle)
      plan = Plan OneForOne (Intensity 1000 10)
        [ Child (T.pack ("worker " <> show i)) (consumeAt orderBox job)
        | i <- [1 .. workers] ]
      everyone = Set.fromList (map orderNo todo)
      -- Done means "settled for good", not "the work was written down".
      -- 終わりの合図は「仕事を記録した」ではなく「もう動かない」
      finished = do
        settled <- readIORef settledRef
        dead    <- readIORef buriedRef
        pure (Set.union settled (Set.fromList dead) == everyone)
  -- If the supervisor gives up, or the keeper of the graves falls, say so
  -- now instead of waiting forever.
  -- 監督役があきらめたり、墓場の番人が倒れたりしたら、待ち続けずに伝える
  withAsync (supervise plan) $ \boss ->
    withAsync (consumeAt graves bury) $ \keeper -> do
      link boss
      link keeper
      waitUntil finished
  Report <$> readIORef doneRef <*> readIORef buriedRef
-- ANCHOR_END: runDay

-- 🎲 Read the next answer from the tape of bad luck.
-- 不運のテープから、次の答えを読む
nextLuck :: [Bool] -> ([Bool], Bool)
nextLuck falls = (drop 1 falls, take 1 falls == [True])

-- ⏳ Check every 10 ms until it is true.
-- 10 ミリ秒ごとに、本当になるまで確かめる
waitUntil :: IO Bool -> IO ()
waitUntil ready = do
  ok <- ready
  if ok then pure () else threadDelay 10000 >> waitUntil ready

-- ANCHOR_END: all
