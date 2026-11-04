-- ANCHOR: all
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- 🏋️ Answers for the training: filter the roster by job.
-- トレーニングの解答：職業で名簿を絞り込む
module Guild.ApiAnswers (JobApi, jobServer, jobApp) where

import Control.Monad.IO.Class (liftIO)
import Data.Text              (Text)
import Servant

import Guild.Store
import Guild.Types

-- ANCHOR: job
-- 🧭 GET /adventurers?job=Mage -> only the mages (all, without ?job).
-- GET /adventurers?job=Mage -> 魔法使いだけ（?job がなければ全員）
type JobApi =
  "adventurers" :> QueryParam "job" Text :> Get '[JSON] [Adventurer]

jobServer :: RosterStore -> Server JobApi
jobServer store wanted = do
  everyone <- liftIO store.listAdventurers
  pure (case wanted of
          Nothing  -> everyone
          Just job -> filter (\a -> a.job == job) everyone)

jobApp :: RosterStore -> Application
jobApp store = serve (Proxy :: Proxy JobApi) (jobServer store)
-- ANCHOR_END: job
-- ANCHOR_END: all
