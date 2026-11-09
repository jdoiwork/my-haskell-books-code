-- ANCHOR: imports
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy
import Network.HTTP.Types (methodPost)
import Network.Wai.Test (SResponse)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Board.Server (app)
import Board.Store (memoryStore)
-- ANCHOR_END: imports

-- ANCHOR: tests
-- 📮 POST with a JSON body (hspec-wai's post sends no Content-Type).
-- JSON を付けて POST する（hspec-wai の post は Content-Type を付けない）
postJson :: ByteString -> ByteString -> WaiSession st SResponse
postJson path =
  request methodPost (toStrict path) [("Content-Type", "application/json")]
  where
    toStrict = Data.ByteString.Lazy.toStrict

main :: IO ()
main = hspec $ with (flip app "test" <$> memoryStore) $ do
  describe "the quest API" $ do
    -- 最初は空
    it "starts empty" $
      get "/api/quests" `shouldRespondWith` [json|[]|]
    -- 依頼を貼ると、201 で依頼が返る
    it "posts a quest" $
      postJson "/api/quests" [json|{"title": "slay the slime", "reward": 50}|]
        `shouldRespondWith`
          [json|{"questId": 1, "title": "slay the slime",
                 "reward": 50, "done": false}|] { matchStatus = 201 }
    -- 門番が、空の題を断る
    it "rejects an empty title" $
      postJson "/api/quests" [json|{"title": " ", "reward": 50}|]
        `shouldRespondWith`
          [json|{"message": "the title is empty"}|] { matchStatus = 400 }
    -- ない依頼は 404
    it "404 for a quest that is not there" $
      postJson "/api/quests/9/done" ""
        `shouldRespondWith` 404
  -- ANCHOR_END: tests
