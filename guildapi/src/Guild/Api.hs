-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- 🗺️ The API, written as a type: what paths there are, and what each
--    one takes and gives.
-- API を、型で書く：どんな道（パス）があり、それぞれが何を受け取り、
-- 何を返すか
module Guild.Api (RosterApi, rosterApi, openApiDoc) where

import Data.OpenApi    (Info (..), OpenApi (..))
import Data.Proxy      (Proxy (..))
import Data.Text       (Text)
import Servant.API
import Servant.OpenApi (toOpenApi)

import Guild.Types
-- ANCHOR_END: imports

-- ANCHOR: api
-- 🗺️ GET    /adventurers                  -> every adventurer
--    GET    /adventurers/{name}           -> one adventurer
--    POST   /adventurers                  -> add one (201 Created)
--    POST   /adventurers/{name}/levelup   -> one level up
type RosterApi =
  "adventurers" :>
    (    Get '[JSON] [Adventurer]
    :<|> Capture "name" Text :> Get '[JSON] Adventurer
    :<|> ReqBody '[JSON] NewAdventurer :> PostCreated '[JSON] Adventurer
    :<|> Capture "name" Text :> "levelup" :> Post '[JSON] Adventurer
    )

rosterApi :: Proxy RosterApi
rosterApi = Proxy
-- ANCHOR_END: api

-- ANCHOR: openapi
-- 📖 The OpenAPI document, made from the API type,
--    with a title and a version.
-- API の型から作る、OpenAPI のドキュメント（題名と版を付けて）
openApiDoc :: OpenApi
openApiDoc = doc { _openApiInfo = info { _infoTitle = "Guild API"
                                       , _infoVersion = "1.0" } }
  where
    doc  = toOpenApi rosterApi
    info = _openApiInfo doc
-- ANCHOR_END: openapi
-- ANCHOR_END: all
