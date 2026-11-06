-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- 🧪 Tests of the quest board API, on a real SQLite in memory.
-- 依頼の掲示板の API のテスト。メモリー上の本物の SQLite で
module Main (main) where

import Database.SQLite.Simple (open)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON    (json)

import Board.Api   (app)
import Board.Store (migrate)

main :: IO ()
main = hspec $ with freshApp $ do
  describe "POST /quests" $ do
    -- 「貼り出すと 201 と、番号の付いた依頼が返る」
    it "posts a quest with 201" $
      postJson "/quests" [json|{title: "Slay the slime", reward: 100}|]
        `shouldRespondWith`
          [json|{questId: 1, title: "Slay the slime", reward: 100,
                 taker: null}|] { matchStatus = 201 }
    -- 「改行の入った題名は、門で断る」
    it "turns away a title with a newline" $
      postJson "/quests" [json|{title: "a\nb", reward: 100}|]
        `shouldRespondWith` 400
    -- 「報酬が 0 以下なら断る」
    it "turns away a reward of 0" $
      postJson "/quests" [json|{title: "Free work", reward: 0}|]
        `shouldRespondWith`
          [json|{message: "the reward must be positive"}|]
            { matchStatus = 400 }

  describe "POST /quests/{id}/take" $ do
    -- 「受けた人が入った依頼が返る」
    it "lets an adventurer take a quest" $ do
      _ <- postJson "/quests" slime
      postJson "/quests/1/take" [json|{adventurer: "Lambda"}|]
        `shouldRespondWith`
          [json|{questId: 1, title: "Slay the slime", reward: 100,
                 taker: "Lambda"}|]
    -- 「もう受けられた依頼は 409」
    it "says 409 when the quest is already taken" $ do
      _ <- postJson "/quests" slime
      _ <- postJson "/quests/1/take" [json|{adventurer: "Lambda"}|]
      postJson "/quests/1/take" [json|{adventurer: "Rust"}|]
        `shouldRespondWith`
          [json|{message: "the quest is already taken"}|]
            { matchStatus = 409 }
    -- 「ない依頼は 404」
    it "says 404 for a quest that is not there" $
      postJson "/quests/9/take" [json|{adventurer: "Lambda"}|]
        `shouldRespondWith` 404
  where
    -- A new in-memory database for each test.
    -- テストごとに、新しいメモリー上のデータベース
    freshApp = do
      conn <- open ":memory:"
      migrate conn
      pure (app conn)
    slime = [json|{title: "Slay the slime", reward: 100}|]
    postJson path body =
      request "POST" path [("Content-Type", "application/json")] body
-- ANCHOR_END: all
