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
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, encode)
import GHC.Generics (Generic)
import Servant
import Servant.Client (AsClientT, ClientM, client)
import Servant.Server.Generic (AsServer)

import Tsundoku.Api.Json (Problem (..))
import Tsundoku.Api.Server (verdict)
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

-- 🪟 The handler asks the window, as every other one does.
-- ハンドラーは、ほかと同じく窓口に聞くだけ
episodeServer :: Shelf -> EpisodeRoutes AsServer
episodeServer shelf = EpisodeRoutes
  { addEpisodes = \wid new -> do
      result <- liftIO (shelf.intake.addEpisodes (WorkId wid) new.howMany)
      case result of
        Right ()     -> pure NoContent
        Left mistake -> do
          let (status, word) = verdict mistake
          throwError status
            { errBody = encode (Problem word (explain mistake)) }
  }

-- 📞 And the client, from the same record.
-- クライアントも、同じレコードから
episodeClient :: EpisodeRoutes (AsClientT ClientM)
episodeClient = client (Proxy @(NamedRoutes EpisodeRoutes))

episodeApp :: Shelf -> Application
episodeApp shelf =
  serve (Proxy @(NamedRoutes EpisodeRoutes)) (episodeServer shelf)
-- ANCHOR_END: all
