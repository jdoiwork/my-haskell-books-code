-- ANCHOR: all
-- | Functions that work in any monad with IO at the bottom.
-- いちばん下に IO があるモナドなら、どこでも動く関数
module Lift.Anywhere
  ( announceIO
  , announce
  , logAny
  , questDay2
  ) where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Lift.App
import Lift.Stack (QuestError (..))

-- ANCHOR: announce
-- 📣 Only for IO.
-- IO 専用
announceIO :: Text -> IO ()
announceIO msg = TIO.putStrLn ("[board] " <> msg)

-- 📣 For any monad that can do IO (it has MonadIO).
-- IO ができるモナドなら、どれでも（MonadIO を持っていれば）
announce :: MonadIO m => Text -> m ()
announce msg = liftIO (TIO.putStrLn ("[board] " <> msg))
-- ANCHOR_END: announce

-- ANCHOR: logAny
-- 📝 Write a log line in any monad that can read the Env and do IO.
-- Env を読めて IO もできるモナドなら、どこでもログを1行
logAny :: (MonadReader Env m, MonadIO m) => Text -> m ()
logAny line = do
  write <- asks envLog
  liftIO (write line)

-- 🏢 The three floors again, with no lift for the log or the board.
-- また3階建て。ログにも掲示板にも、lift はいらない
questDay2 :: [(Text, Int)] -> StateT Int (ExceptT QuestError App) ()
questDay2 quests = do
  for_ quests $ \(quest, reward) -> do
    when (reward <= 0) $
      throwError (NoReward quest)
    logAny ("accepted: " <> quest)
    announce quest
    modify' (+ reward)
  total <- get
  logAny ("total: " <> T.pack (show total) <> " G")
-- ANCHOR_END: logAny
-- ANCHOR_END: all
