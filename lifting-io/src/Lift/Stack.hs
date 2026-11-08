-- ANCHOR: all
-- | A stack with three floors: StateT over ExceptT over App.
--   lift goes down one floor; liftIO goes all the way down to IO.
-- 3階建てのモナド：App の上の ExceptT の上の StateT。
-- lift は1階下へ、liftIO はいちばん下の IO まで一気に
module Lift.Stack
  ( QuestError (..)
  , acceptQuest
  , questDay
  , runQuestDay
  ) where

-- ANCHOR: imports
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Lift.App
-- ANCHOR_END: imports

-- ANCHOR: accept
-- 🚫 Why a quest could not be accepted.
-- 依頼を受けられなかった理由
data QuestError = NoReward Text
  deriving (Show, Eq)

-- 📜 Two floors: ExceptT over App.
-- 2階建て：App の上の ExceptT
acceptQuest :: Text -> Int -> ExceptT QuestError App Int
acceptQuest quest reward = do
  when (reward <= 0) $
    throwError (NoReward quest)
  -- An App step: lift, one floor down.
  -- App の手順：lift で1階下へ
  lift (logInfo ("accepted: " <> quest))
  -- An IO step: liftIO, all the way down.
  -- IO の手順：liftIO で、いちばん下まで
  -- 「依頼：○○（○ G）」
  liftIO (TIO.putStrLn ("Quest: " <> quest <> " (" <> gold reward <> ")"))
  pure reward

gold :: Int -> Text
gold n = T.pack (show n) <> " G"
-- ANCHOR_END: accept

-- ANCHOR: day
-- 🏢 Three floors: StateT Int (the gold of the day) over the two above.
-- 3階建て：上の2階建ての上に、StateT Int（その日の金貨）
questDay :: [(Text, Int)] -> StateT Int (ExceptT QuestError App) ()
questDay quests = do
  for_ quests $ \(quest, reward) -> do
    -- ExceptT is one floor down. / ExceptT は1階下
    got <- lift (acceptQuest quest reward)
    modify' (+ got)
  total <- get
  -- App is two floors down. / App は2階下
  lift (lift (logInfo ("total: " <> gold total)))
  -- IO is at the bottom: one liftIO, however many floors.
  -- IO はいちばん下：何階建てでも、liftIO は1回
  -- 「今日の仕事は、おしまい」
  liftIO (TIO.putStrLn "The day is over.")

-- 🔑 Peel the floors from the outside in.
-- 外側から順に、皮をむく
runQuestDay :: Env -> [(Text, Int)] -> IO (Either QuestError Int)
runQuestDay env quests =
  runApp env (runExceptT (execStateT (questDay quests) 0))
-- ANCHOR_END: day
-- ANCHOR_END: all
