-- ANCHOR: all
-- | State and boxes together: each clerk keeps a tally in State, takes
--   tickets from the shared box, and adds the tally to the total at the end.
-- State と箱を一緒に使う：受付係は State で自分の集計をつけ、番号札は
-- みんなの箱から取り、最後に集計を全体の合計に足す
module Shift
  ( Tally (..)
  , serveOne
  , clerkShift
  , shiftDemo
  ) where

import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.STM
import Control.Monad.State.Strict
import Data.IORef
import Data.List (sort)
import Data.Text (Text)

import Tickets (newTicketBox, takeTicket)

-- ANCHOR: tally
-- 🧮 One clerk's tally: how many served, and their ticket numbers.
-- 受付係1人の集計：何人を迎えたかと、その番号札
data Tally = Tally
  { served  :: Int
  , tickets :: [Int]
  }
  deriving (Show)

-- Tallies can be added up: the counts are added, the lists joined.
-- 集計は足し合わせられる：人数は足し、番号はつなぐ
instance Semigroup Tally where
  Tally a xs <> Tally b ys = Tally (a + b) (xs ++ ys)

instance Monoid Tally where
  mempty = Tally 0 []
-- ANCHOR_END: tally

-- ANCHOR: serve
-- 🙋 Serve one visitor: the ticket comes from the shared box (IO),
--    the tally is carried in State.
-- 1人を迎える：番号札はみんなの箱から（IO）、集計は State で持ち回る
serveOne :: IORef Int -> Text -> StateT Tally IO ()
serveOne box _name = do
  n <- liftIO (takeTicket box)
  modify' (\t -> t { served = served t + 1, tickets = tickets t ++ [n] })
-- ANCHOR_END: serve

-- ANCHOR: shift
-- 🧑‍💼 A clerk's shift: tally in State, then add it to the total (TVar).
-- 受付係の勤務：State で集計して、最後に合計（TVar）に足す
clerkShift :: IORef Int -> TVar Tally -> [Text] -> IO ()
clerkShift box total visitors = do
  mine <- execStateT (mapM_ (serveOne box) visitors) mempty
  atomically (modifyTVar' total (<> mine))

-- 🧪 Three clerks, three visitors each, at the same time.
-- 受付係3人が、3人ずつ、同時に迎える
shiftDemo :: IO (Int, [Int])
shiftDemo = do
  box   <- newTicketBox
  total <- newTVarIO mempty
  forConcurrently_
    [ ["Lambda", "Python", "Rust"]
    , ["Ruby", "Go", "Erlang"]
    , ["Kotlin", "Lisp", "GHC"]
    ]
    (clerkShift box total)
  Tally n ts <- readTVarIO total
  pure (n, sort ts)
-- ANCHOR_END: shift
-- ANCHOR_END: all
