-- ANCHOR: all
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- | Training, step 2: a route for "new episodes came out". On its own
--   record here, so that the chapter's API stays as it is.
-- トレーニングのステップ2：「新しい話が出た」の道。章の API を変えない
-- ように、ここでは別のレコードにした
module ShelfAnswers.Episodes
  ( NewEpisodes (..)
  , EpisodeRoutes (..)
  , episodeServer
  , episodeClient
  , episodeApp
  , episodeLimit
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Servant
import Servant.Client (AsClientT, ClientM, client)
import Servant.Server.Generic (AsServer)

import Tsundoku.Api.Json (Problem (..))
import Tsundoku.Api.Server (problemError, verdict)
import Tsundoku.Form (explain)
import Tsundoku.Shelf
import Tsundoku.Types

-- 🌱 How many new episodes came out.
-- 新しく出た話の数
newtype NewEpisodes = NewEpisodes { howMany :: Int }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 🧭 POST /works/{id}/episodes
data EpisodeRoutes mode = EpisodeRoutes
  { addEpisodes :: mode :- "works" :> Capture "id" Int :> "episodes"
                         :> ReqBody '[JSON] NewEpisodes
                         :> PostNoContent
  }
  deriving stock Generic

-- 📏 How many episodes may come out at once. As a route, the number
--    comes from outside, and the window adds the episodes one by one,
--    so a huge number would keep the shelf busy for minutes.
-- 一度に出る話の数の上限。道にすると、この数は外から来る。窓口は
-- 1話ずつ足すので、大きな数だと、本棚が何分もふさがる
episodeLimit :: Int
episodeLimit = 1000

-- 🪟 The handler checks the number, then asks the window, as every
--    other one does.
-- ハンドラーは、数を確かめてから、ほかと同じく窓口に聞く
episodeServer :: Shelf -> EpisodeRoutes AsServer
episodeServer shelf = EpisodeRoutes
  { addEpisodes = \wid new -> do
      when (new.howMany < 1 || new.howMany > episodeLimit) $
        throwError (problemError err422 (Problem "bad-count"
          ["新しい話の数は、1〜" <> T.pack (show episodeLimit)
             <> " です"]))
      result <- liftIO (shelf.addEpisodes (WorkId wid) new.howMany)
      case result of
        Right ()     -> pure NoContent
        Left mistake -> do
          let (status, word) = verdict mistake
          throwError
            (problemError status (Problem word (explain mistake)))
  }

-- 📞 And the client, from the same record.
-- クライアントも、同じレコードから
episodeClient :: EpisodeRoutes (AsClientT ClientM)
episodeClient = client (Proxy @(NamedRoutes EpisodeRoutes))

episodeApp :: Shelf -> Application
episodeApp shelf =
  serve (Proxy @(NamedRoutes EpisodeRoutes)) (episodeServer shelf)
-- ANCHOR_END: all
