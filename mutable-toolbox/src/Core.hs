-- ANCHOR: all
-- | One pure core, four boxes: State, IORef, MVar and TVar.
-- 純粋な核を1つ書いて、4つの箱（State・IORef・MVar・TVar）に差し込む
module Core
  ( Board (..)
  , emptyBoard
  , pinQuest
  , pinState
  , pinIORef
  , pinMVar
  , pinTVar
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.State.Strict (State, state)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Tuple (swap)

-- ANCHOR: core
-- 📋 The quest board: the next number, and the quests on it.
-- 依頼の掲示板：次の番号と、貼ってある依頼
--    The fields are strict (!): a board in a box is a worked-out board.
-- フィールドは正格（!）：箱の中の掲示板は、計算ずみの掲示板
data Board = Board
  { nextId :: !Int
  , quests :: !(Map Int Text)
  }
  deriving (Show)

emptyBoard :: Board
emptyBoard = Board { nextId = 1, quests = Map.empty }

-- 📌 Pin a quest: the answer is its number, and the board after it.
-- 依頼を貼る：答えはその番号と、貼ったあとの掲示板
pinQuest :: Text -> Board -> (Int, Board)
pinQuest title board = (n, board')
  where
    n      = nextId board
    board' = Board
      { nextId = n + 1
      , quests = Map.insert n title (quests board)
      }
-- ANCHOR_END: core

-- ANCHOR: boxes
-- 🧺 In State: carry the board from step to step.
-- State で：掲示板を、手順から手順へ持ち回る
pinState :: Text -> State Board Int
pinState title = state (pinQuest title)

-- 📦 In an IORef. The pair is the other way round: (new board, answer).
-- IORef で。組の順番が逆：（新しい掲示板, 答え）
pinIORef :: IORef Board -> Text -> IO Int
pinIORef box title = atomicModifyIORef' box (swap . pinQuest title)

-- 🔒 In an MVar. This one is the other way round, too.
-- MVar で。こちらも逆
pinMVar :: MVar Board -> Text -> IO Int
pinMVar box title = modifyMVar box $ \board -> do
  let (n, board') = pinQuest title board
  -- Work out the new board before it goes back into the box (seq).
  -- 新しい掲示板を計算してから、箱に戻す（seq）
  board' `seq` pure (board', n)

-- ⚛️ In a TVar. The same order as State.
-- TVar で。State と同じ順番
pinTVar :: TVar Board -> Text -> STM Int
pinTVar box title = stateTVar' box (pinQuest title)

-- 🧮 Like stateTVar, but the new state is worked out before it goes
--    into the TVar (stateTVar puts in the recipe, not the result).
-- stateTVar と同じだが、新しい状態を計算してから TVar に入れる
-- （stateTVar は、計算した結果ではなく、作り方のまま入れる）
stateTVar' :: TVar s -> (s -> (a, s)) -> STM a
stateTVar' var f = do
  (a, new) <- f <$> readTVar var
  writeTVar var $! new
  pure a
-- ANCHOR_END: boxes
-- ANCHOR_END: all
