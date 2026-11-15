-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

-- 🌐 The quest board as a JSON API.
-- 依頼の掲示板を、JSON の API に
module Board.Api (BoardApi, app) where

import Control.Concurrent.MVar (MVar, withMVar)
import Control.Monad.IO.Class  (liftIO)
import Data.Aeson              (FromJSON, ToJSON, encode, object, (.=))
import Data.Text               (Text)
import Data.Text               qualified as T
import Database.SQLite.Simple  (Connection)
import GHC.Generics            (Generic)
import Servant

import Board.Store
import Board.Title
-- ANCHOR_END: imports

-- ANCHOR: types
-- ✍️ What a client sends: a new quest, and who takes a quest.
-- クライアントが送るもの：新しい依頼と、依頼を受ける人
data NewQuest = NewQuest { title :: QuestTitle, reward :: Int }
  deriving stock    (Generic)
  deriving anyclass (FromJSON)

newtype TakeRequest = TakeRequest { adventurer :: Text }
  deriving stock    (Generic)
  deriving anyclass (FromJSON, ToJSON)
-- ANCHOR_END: types

-- ANCHOR: api
-- 🗺️ GET  /quests            -> every quest
--    POST /quests            -> post one (201 Created)
--    POST /quests/{id}/take  -> take one (404, 409)
type BoardApi =
  "quests" :>
    (    Get '[JSON] [Quest]
    :<|> ReqBody '[JSON] NewQuest :> PostCreated '[JSON] Quest
    :<|> Capture "id" Int :> "take" :> ReqBody '[JSON] TakeRequest
           :> Post '[JSON] Quest
    )
-- ANCHOR_END: api

-- ANCHOR: server
-- 🏰 The handlers, in the same order as the API type.
--    The connection is in an MVar: one request at a time uses it.
-- API の型と同じ順のハンドラー。
-- 接続は MVar に入れ、1度に1つの要求だけが使う
server :: MVar Connection -> Server BoardApi
server box = listAll :<|> post :<|> takeOne
  where
    withConn :: (Connection -> IO a) -> Handler a
    withConn action = liftIO (withMVar box action)

    listAll = withConn listQuests

    post (NewQuest t gold)
      | gold <= 0 = failWith err400 "the reward must be positive"
      | otherwise = withConn (\conn -> postQuest conn t gold)

    takeOne qid (TakeRequest who) = do
      result <- withConn (\conn -> takeQuest conn qid who)
      case result of
        Right q           -> pure q
        Left NoSuchQuest  -> failWith err404 "no such quest"
        Left AlreadyTaken -> failWith err409 "the quest is already taken"

-- 🚫 Stop with an HTTP error, and a JSON body {"message": "..."}.
-- HTTP のエラーで止める。中身は JSON の {"message": "..."}
failWith :: ServerError -> Text -> Handler a
failWith err message = throwError (jsonError err message)

jsonError :: ServerError -> Text -> ServerError
jsonError err message =
  err
    { errBody    = encode (object ["message" .= message])
    , errHeaders = [("Content-Type", "application/json")]
    }

-- 📮 When servant itself turns a request away (a broken JSON, a title
--    that fails the gatekeeper, a path that is not there), answer in
--    the same JSON too.
-- servant 自身が断るとき（壊れた JSON、門番に断られた題名、ない道）も、
-- 同じ JSON で答える
jsonFormatters :: ErrorFormatters
jsonFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = \_ _ e -> jsonError err400 (T.pack e)
    , urlParseErrorFormatter   = \_ _ e -> jsonError err400 (T.pack e)
    , notFoundErrorFormatter   = \_     -> jsonError err404 "no such path"
    }

app :: MVar Connection -> Application
app box =
  serveWithContext (Proxy @BoardApi) (jsonFormatters :. EmptyContext)
    (server box)
-- ANCHOR_END: server
-- ANCHOR_END: all
