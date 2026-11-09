-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NoFieldSelectors #-}
-- | The API as a type: records of routes (servant's NamedRoutes). The
--   server and the client are both made from these two records.
-- 型で書いた API：道（ルート）のレコード（servant の NamedRoutes）。
-- サーバーもクライアントも、この2つのレコードから作る
module Tsundoku.Api.Routes
  ( ShelfRoutes (..)
  , WorkRoutes (..)
  , ShelfApi
  , shelfApi
  ) where

import Data.Map.Strict (Map)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API

import Tsundoku.Api.Json
-- ANCHOR_END: header

-- ANCHOR: routes
-- 🧭 The whole shelf: the list, adding, one work, and searching.
-- 本棚の全体：一覧、足す、作品1つ、探す
data ShelfRoutes mode = ShelfRoutes
  { listWorks :: mode :- "works" :> Get '[JSON] [WorkSummary]
  , addWork   :: mode :- "works" :> ReqBody '[JSON] NewWork
                                 :> PostCreated '[JSON] CardDetail
  , work      :: mode :- "works" :> Capture "id" Int
                                 :> NamedRoutes WorkRoutes
  , search    :: mode :- "search" :> QueryParam' '[Required, Strict] "q" Text
                                  :> Get '[JSON] [HitItem]
  }
  deriving stock Generic

-- 📖 One work: /works/{id}/... Every route here already knows which work.
-- 作品1つ：/works/{id}/…。ここの道は、どれも、どの作品かを知っている
data WorkRoutes mode = WorkRoutes
  { card       :: mode :- Get '[JSON] CardDetail
  , revise     :: mode :- ReqBody '[JSON] EditedWork
                                    :> Put '[JSON] CardDetail
  , remove     :: mode :- DeleteNoContent
  , happen     :: mode :- "reading" :> ReqBody '[JSON] Happened
                                    :> Post '[JSON] ReadingNow
  , addNote    :: mode :- "notes"   :> ReqBody '[JSON] NewNote
                                    :> PostCreated '[JSON] NoteItem
  , logReading :: mode :- "log"     :> ReqBody '[JSON] ReadOn
                                    :> PostNoContent
  , rate       :: mode :- "rating"  :> ReqBody '[JSON] Points
                                    :> PutNoContent
  , rateEntry  :: mode :- "entries" :> Capture "entry" Int :> "rating"
                                    :> ReqBody '[JSON] Points
                                    :> PutNoContent
  , setText    :: mode :- "entries" :> Capture "entry" Int :> "text"
                                    :> ReqBody '[PlainText] Text
                                    :> PutNoContent
  , form       :: mode :- "form"    :> Get '[JSON] [FormField]
  , editAttrs  :: mode :- "attrs"   :> ReqBody '[JSON] (Map Text Text)
                                    :> Put '[JSON] [FormField]
  }
  deriving stock Generic

-- 🏷️ The API as one type, and a Proxy to carry it around.
-- API の全体を1つの型に。それを持ち運ぶ Proxy
type ShelfApi = NamedRoutes ShelfRoutes

shelfApi :: Proxy ShelfApi
shelfApi = Proxy
-- ANCHOR_END: routes
-- ANCHOR_END: all
