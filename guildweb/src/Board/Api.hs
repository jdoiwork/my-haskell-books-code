-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE DataKinds #-}

-- 🗺️ The API as a type, and the whole site: the API plus the files
--    of the React app.
-- API を型で。サイト全体は、API と React のアプリのファイル
module Board.Api (QuestApi, SiteApi, siteApi) where

import Data.Proxy (Proxy (..))
import Servant.API

import Board.Types
-- ANCHOR_END: imports

-- ANCHOR: api
-- 🗺️ GET  /api/quests            -> every quest
--    POST /api/quests            -> post one (201 Created)
--    POST /api/quests/{id}/done  -> finish one
type QuestApi =
  "api" :> "quests" :>
    (    Get '[JSON] [Quest]
    :<|> ReqBody '[JSON] NewQuest :> PostCreated '[JSON] Quest
    :<|> Capture "id" Int :> "done" :> Post '[JSON] Quest
    )

-- 🌐 Everything else is a file of the built React app.
-- それ以外は、ビルドした React のアプリのファイル
type SiteApi = QuestApi :<|> Raw

siteApi :: Proxy SiteApi
siteApi = Proxy
-- ANCHOR_END: api
-- ANCHOR_END: all
