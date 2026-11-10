-- ANCHOR: all
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- | Training, step 2: more works alike, on a route of their own:
--   GET /works/{id}/alike?n=5. The routes already there do not change.
--   n is 1 to 20; any other n never reaches the handler (servant
--   answers 400), as the limit of the API chapter.
-- トレーニングのステップ2：似ている作品を、いくつでも。道を1本足す：
-- GET /works/{id}/alike?n=5。今ある道は変えない。n は 1〜20。それ以外は
-- ハンドラーまで届かない（servant が 400 を返す。API の章の limit と同じ）
module ShelfAnswers.Alike
  ( HowMany
  , howMany
  , AlikeRoutes (..)
  , alikeApp
  , alikeClient
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Servant
import Servant.Client (AsClientT, ClientM, client)
import Servant.Server.Generic (AsServer)

import Tsundoku.Api.Server (refuse)
import Tsundoku.Api.Site (AlikeItem (..), percentOf)
import Tsundoku.Card (Card (..))
import Tsundoku.Review (similarTo)
import Tsundoku.Shelf
import Tsundoku.Types

-- 🔢 How many works alike to show (1 to 20). Only howMany makes one.
-- 似ている作品を、いくつ見せるか（1〜20）。作れるのは howMany だけ
newtype HowMany = HowMany Int deriving (Show, Eq)

howMany :: Int -> Either T.Text HowMany
howMany n
  | n >= 1 && n <= 20 = Right (HowMany n)
  | otherwise         = Left "n: 1 から 20 まで"

instance FromHttpApiData HowMany where
  parseQueryParam t = parseQueryParam t >>= howMany

instance ToHttpApiData HowMany where
  toQueryParam (HowMany n) = toQueryParam n

-- 🧭 GET /works/{id}/alike?n=5
data AlikeRoutes mode = AlikeRoutes
  { alikeTo :: mode :- "works" :> Capture "id" Int :> "alike"
                     :> QueryParam "n" HowMany
                     :> Get '[JSON] [AlikeItem]
  }
  deriving stock Generic

-- 🤝 No n: 3, as the extras give. The core finds them (similarTo);
--    the route only says how many.
-- n がなければ、おまけと同じ3つ。探すのは核（similarTo）で、道は数を
-- 言うだけ
alikeServer :: Shelf -> AlikeRoutes AsServer
alikeServer shelf = AlikeRoutes
  { alikeTo = \i asked -> do
      cards <- liftIO shelf.works.listCards
      let wid         = WorkId i
          HowMany n   = fromMaybe (HowMany 3) asked
      case lookup wid cards of
        Nothing -> refuse (NoSuchWork wid)
        Just _  -> pure [ AlikeItem m c.work.title (percentOf r)
                        | (r, WorkId m, c) <- similarTo n wid cards ]
  }

alikeApp :: Shelf -> Application
alikeApp shelf = serve (Proxy @(NamedRoutes AlikeRoutes)) (alikeServer shelf)

alikeClient :: AlikeRoutes (AsClientT ClientM)
alikeClient = client (Proxy @(NamedRoutes AlikeRoutes))
-- ANCHOR_END: all
