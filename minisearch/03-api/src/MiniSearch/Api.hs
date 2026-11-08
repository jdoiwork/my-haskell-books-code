-- ANCHOR: head
-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | The search engine as a JSON Web API.
-- 検索エンジンを、JSON の Web API にする。
module MiniSearch.Api
  ( SearchApi
  , HitView (..)
  , SearchResponse (..)
  , DocumentView (..)
  , app
  ) where

import Data.Aeson (FromJSON, ToJSON, encode, object, (.=))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant

import MiniSearch.Query
import MiniSearch.Search (searchTokens)
import MiniSearch.Snippet (snippet)
import MiniSearch.Types
-- ANCHOR_END: head

-- ANCHOR: views
-- 🎯 One hit, as the client sees it.
-- クライアントから見た、ヒットひとつ
data HitView = HitView
  { hitId   :: Int
  , title   :: Text
  , score   :: Double
  , excerpt :: Text
  }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 📦 The answer to a search.
-- 検索への答え
data SearchResponse = SearchResponse
  { query :: Text
  , total :: Int        -- all the matches / 当たった数の全部
  , hits  :: [HitView]  -- up to the limit / 件数の上限まで
  }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 📄 One document.
-- 文書ひとつ
data DocumentView = DocumentView
  { documentId :: Int
  , title      :: Text
  , body       :: Text
  }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
-- ANCHOR_END: views

-- ANCHOR: api
-- 🗺️ GET /search?q=dragon+fire&limit=5
--    GET /docs/{id}
type SearchApi =
       "search" :> QueryParam "q" Text :> QueryParam "limit" Int
         :> Get '[JSON] SearchResponse
  :<|> "docs" :> Capture "id" Int :> Get '[JSON] DocumentView
-- ANCHOR_END: api

-- ANCHOR: server
-- 🏰 The handlers, in the same order as the API type.
-- API の型と同じ順のハンドラー
server :: Index -> Server SearchApi
server index = searchHandler :<|> documentHandler
  where
    searchHandler mq mlimit =
      case (,) <$> parseSearchQuery (fromMaybe "" mq)
               <*> parseLimit mlimit of
        Left err -> failWith err400 (renderQueryError err)
        Right (q, lim) -> do
          let found = searchTokens index (queryTokens q)
              shown = take (limitValue lim) found
          pure (SearchResponse (queryText q) (length found)
                               (map (toView (queryTokens q)) shown))

    documentHandler n =
      case Map.lookup (DocId n) (indexDocs index) of
        -- 「その番号の文書はありません」
        Nothing  -> failWith err404 "no such document"
        Just doc -> pure (DocumentView n (docTitle doc) (docBody doc))

    toView tokens h =
      let DocId n = hitDoc h
          body'   = maybe "" docBody (Map.lookup (hitDoc h) (indexDocs index))
      in HitView n (hitTitle h) (hitScore h) (snippet 5 tokens body')
-- ANCHOR_END: server

-- ANCHOR: app
-- 🚫 Stop with an HTTP error, and a JSON body {"message": "..."}.
-- HTTP のエラーで止める。中身は JSON の {"message": "..."}
failWith :: ServerError -> Text -> Handler a
failWith err message =
  throwError err
    { errBody    = encode (object ["message" .= message])
    , errHeaders = [("Content-Type", "application/json")]
    }

-- 🌐 The Web application.
-- Web アプリ
app :: Index -> Application
app index = serve (Proxy @SearchApi) (server index)
-- ANCHOR_END: app
-- ANCHOR_END: all
