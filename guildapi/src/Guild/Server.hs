-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- 🏰 The handlers: what each path does, using a roster store.
-- ハンドラー：それぞれの道ですることを、名簿の置き場を使って書く
module Guild.Server (server, app) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson             (encode)
import Data.OpenApi           (OpenApi)
import Data.Text              (Text)
import Servant

import Guild.Api
import Guild.Store
import Guild.Types

-- ANCHOR: server
-- 🏰 One handler for each path, in the same order as in the API type.
-- API の型と同じ順に、道ごとのハンドラーを並べる
server :: RosterStore -> Server RosterApi
server store = listAll :<|> findOne :<|> addOne :<|> levelUpOne
  where
    listAll :: Handler [Adventurer]
    listAll = liftIO store.listAdventurers

    findOne :: Text -> Handler Adventurer
    findOne who = do
      found <- liftIO (store.findAdventurer who)
      maybe (failWith err404 "no such adventurer") pure found

    addOne :: NewAdventurer -> Handler Adventurer
    addOne new = do
      added <- liftIO (store.addAdventurer new)
      maybe (failWith err409 "the name is taken") pure added

    levelUpOne :: Text -> Handler Adventurer
    levelUpOne who = do
      upped <- liftIO (store.levelUp who)
      maybe (failWith err404 "no such adventurer") pure upped
-- ANCHOR_END: server

-- ANCHOR: fail
-- 🚫 Stop with an HTTP error, and a JSON body {"message": "..."}.
-- HTTP のエラーで止める。中身は JSON の {"message": "..."}
failWith :: ServerError -> Text -> Handler a
failWith err message =
  throwError err
    { errBody    = encode (ApiError message)
    , errHeaders = [("Content-Type", "application/json")]
    }
-- ANCHOR_END: fail

-- ANCHOR: app
-- 🌐 The whole web application: the API, plus /openapi.json.
-- Web アプリケーションの全体：API と /openapi.json
type WholeApi = RosterApi :<|> "openapi.json" :> Get '[JSON] OpenApi

app :: RosterStore -> Application
app store = serve (Proxy @WholeApi) (server store :<|> pure openApiDoc)
-- ANCHOR_END: app
-- ANCHOR_END: all
