-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

-- 🌐 The quest board as a JSON API.
-- 依頼の掲示板を、JSON の API に
module Board.Api (BoardApi, app) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson             (FromJSON, ToJSON, encode, object, (.=))
import Data.Text              (Text)
import Database.SQLite.Simple (Connection)
import GHC.Generics           (Generic)
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
-- API の型と同じ順のハンドラー
server :: Connection -> Server BoardApi
server conn = listAll :<|> post :<|> takeOne
  where
    listAll = liftIO (listQuests conn)

    post (NewQuest t gold)
      | gold <= 0 = failWith err400 "the reward must be positive"
      | otherwise = liftIO (postQuest conn t gold)

    takeOne qid (TakeRequest who) = do
      result <- liftIO (takeQuest conn qid who)
      case result of
        Right q           -> pure q
        Left NoSuchQuest  -> failWith err404 "no such quest"
        Left AlreadyTaken -> failWith err409 "the quest is already taken"

-- 🚫 Stop with an HTTP error, and a JSON body {"message": "..."}.
-- HTTP のエラーで止める。中身は JSON の {"message": "..."}
failWith :: ServerError -> Text -> Handler a
failWith err message =
  throwError err
    { errBody    = encode (object ["message" .= message])
    , errHeaders = [("Content-Type", "application/json")]
    }

app :: Connection -> Application
app conn = serve (Proxy @BoardApi) (server conn)
-- ANCHOR_END: server
-- ANCHOR_END: all
