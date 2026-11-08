-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- 🧪 Tests of the API: send requests to the app, without a real network.
-- API のテスト：本物のネットワークなしで、アプリに要求を送る
module Main (main) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON (json)

import Guild.ApiAnswers (jobApp)
import Guild.Server     (app)
import Guild.Store      (newMemoryStore)
-- ANCHOR_END: imports

-- ANCHOR: spec
main :: IO ()
main = hspec $ do
  rosterSpec
  -- answers for the training / トレーニングの解答
  answersSpec

rosterSpec :: Spec
rosterSpec = with (app <$> newMemoryStore) $ do
  describe "GET /adventurers" $
    -- 「最初は空っぽ」
    it "starts empty" $
      get "/adventurers" `shouldRespondWith` [json|[]|]

  describe "POST /adventurers" $ do
    -- 「登録すると 201 と、番号の付いた冒険者が返る」
    it "adds an adventurer with 201" $
      postJson "/adventurers" lambda `shouldRespondWith`
        [json|{adventurerId: 1, name: "Lambda", job: "Mage", level: 3}|]
          { matchStatus = 201 }
    -- 「同じ名前は 409」
    it "rejects a taken name with 409" $ do
      _ <- postJson "/adventurers" lambda
      postJson "/adventurers" lambda `shouldRespondWith`
        [json|{message: "the name is taken"}|] { matchStatus = 409 }

  describe "GET /adventurers/{name}" $
    -- 「いない名前は 404」
    it "says 404 for an unknown name" $
      get "/adventurers/Nobody" `shouldRespondWith`
        [json|{message: "no such adventurer"}|] { matchStatus = 404 }

  describe "GET /openapi.json" $
    -- 「OpenAPI のドキュメントが返る」
    it "serves the OpenAPI document" $
      get "/openapi.json" `shouldRespondWith` 200
  where
    lambda = [json|{name: "Lambda", job: "Mage", level: 3}|]
    postJson path body =
      request "POST" path [("Content-Type", "application/json")] body
-- ANCHOR_END: spec

answersSpec :: Spec
answersSpec = do
  -- ANCHOR: levelup
  with (app <$> newMemoryStore) $
    describe "POST /adventurers/{name}/levelup" $
      -- 「レベルが1上がった冒険者が返る」
      it "returns the adventurer one level up" $ do
        _ <- postJson "/adventurers" lambda
        request "POST" "/adventurers/Lambda/levelup" [] "" `shouldRespondWith`
          [json|{adventurerId: 1, name: "Lambda", job: "Mage", level: 4}|]
  -- ANCHOR_END: levelup
  with (jobApp <$> newMemoryStore) $
    describe "GET /adventurers?job=..." $
      -- 「?job がなければ、全員（ここでは空っぽ）」
      it "lists everyone without ?job" $
        get "/adventurers" `shouldRespondWith` [json|[]|]
  where
    lambda = [json|{name: "Lambda", job: "Mage", level: 3}|]
    postJson path body =
      request "POST" path [("Content-Type", "application/json")] body
-- ANCHOR_END: all
