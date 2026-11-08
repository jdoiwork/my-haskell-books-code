-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- 🏰 The handlers, and the WAI application for the whole site.
-- ハンドラーと、サイト全体の WAI アプリケーション
module Board.Server (app) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.Text (Text)
import Servant

import Board.Api
import Board.Store
import Board.Types
-- ANCHOR_END: imports

-- ANCHOR: server
-- 🏰 One handler for each path of the API, in the same order.
-- API の道ごとに、同じ順でハンドラーを並べる
questServer :: QuestStore -> Server QuestApi
questServer store = list :<|> post :<|> finish
  where
    list = liftIO store.listQuests

    post new = case checkNewQuest new of
      Left why  -> failWith err400 why
      Right ok  -> liftIO (store.addQuest ok)

    finish n = do
      found <- liftIO (store.finishQuest n)
      maybe (failWith err404 "no such quest") pure found

-- 🚫 Fail with a JSON body that the browser can show.
-- ブラウザーが表示できる JSON を付けて失敗する
failWith :: ServerError -> Text -> Handler a
failWith err why =
  throwError err
    { errBody    = encode (ApiError why)
    , errHeaders = [("Content-Type", "application/json")]
    }
-- ANCHOR_END: server

-- ANCHOR: app
-- 🌐 The API, and the files in the given folder for everything else.
-- API と、それ以外は指定したフォルダーのファイル
app :: QuestStore -> FilePath -> Application
app store staticDir =
  serve siteApi (questServer store :<|> serveDirectoryFileServer staticDir)
-- ANCHOR_END: app
-- ANCHOR_END: all
