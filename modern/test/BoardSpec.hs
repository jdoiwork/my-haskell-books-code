{-# LANGUAGE OverloadedStrings #-}
module BoardSpec (spec) where

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Modern.BoardMcp
import Modern.McpKit
import Modern.QuestBoard
import ModernAnswers (applyOpOneEach, totalRewardTool)

-- 🎲 Any op, with numbers and names that often hit the rules.
-- でたらめな Op。決まりに引っかかりやすい番号と名前にする
newtype AnyOp = AnyOp BoardOp
  deriving stock (Show)

instance Arbitrary AnyOp where
  arbitrary =
    AnyOp <$> oneof
      [ Post   <$> elements titles <*> choose (-5, 100)
      , Take   <$> choose (0, 6) <*> elements ["Lambda", "Python", ""]
      , Finish <$> choose (0, 6)
      ]
    where
      titles = ["Slay slimes", "Find herbs", "", "  "]

boardOf :: [AnyOp] -> Board
boardOf ops = applyAll [ op | AnyOp op <- ops ] emptyBoard

-- 📨 Call a tool of board-mcp and read back (isError, text).
-- board-mcp の道具を呼んで、(isError, テキスト) を読む
callTool :: FilePath -> Text -> Value -> IO (Maybe (Bool, Text))
callTool path = callWith (boardTools path)

callWith :: [Tool] -> Text -> Value -> IO (Maybe (Bool, Text))
callWith tools name arguments = do
  answer <- handleWith "board-mcp" tools $
    object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id"      .= (1 :: Int)
      , "method"  .= ("tools/call" :: Text)
      , "params"  .= object ["name" .= name, "arguments" .= arguments]
      ]
  pure (answer >>= parseMaybe readResult)
  where
    readResult = withObject "answer" $ \o -> do
      r <- o .: "result"
      content <- r .: "content"
      text <- case content of
        first : _ -> first .: "text"
        []        -> fail "no content"
      (,) <$> r .: "isError" <*> pure text

spec :: Spec
spec = do
  describe "Modern.QuestBoard" $ do
    -- 出して、引き受けて、終える
    it "post, take and finish" $
      fmap quests
        (applyOp (Post "Slay slimes" 30) emptyBoard
           >>= applyOp (Take 1 "Lambda")
           >>= applyOp (Finish 1))
        `shouldBe` Right [Quest 1 "Slay slimes" 30 (DoneBy "Lambda")]
    -- 引き受けられた依頼は、もう引き受けられない
    it "a taken quest cannot be taken again" $
      (applyOp (Post "Slay slimes" 30) emptyBoard
         >>= applyOp (Take 1 "Lambda")
         >>= applyOp (Take 1 "Python"))
        `shouldBe` Left "quest #1 is not open"
    -- まだ誰も引き受けていない依頼は、終えられない
    it "an open quest cannot be finished" $
      (applyOp (Post "Find herbs" 10) emptyBoard >>= applyOp (Finish 1))
        `shouldBe` Left "quest #1 has not been taken yet"

    -- ANCHOR: props
    -- 番号は 1 から、すき間なく並ぶ
    prop "numbers go 1, 2, 3, ... without gaps" $ \ops ->
      let board = boardOf ops
      in map questId (quests board) === [1 .. length (quests board)]
    -- ファイルに書いて読み戻すと、元どおり
    prop "the board comes back from JSON" $ \ops ->
      let board = boardOf ops
      in decode (encode board) === Just board
    -- ANCHOR_END: props

  describe "board-mcp" $
    -- 道具で依頼を出せて、断りは「道具のエラー」として届く
    it "posts through a tool, and a refusal is a tool error" $ do
      tmp <- getTemporaryDirectory
      let path = tmp </> "board-mcp-test.json"
      exists <- doesFileExist path
      when exists (removeFile path)
      let slimes = object [ "title"  .= ("Slay slimes" :: Text)
                          , "reward" .= (30 :: Int) ]
      posted <- callTool path "post_quest" slimes
      posted `shouldBe` Just (False, "#1 [open] Slay slimes (30 G)\n")
      refused <- callTool path "finish_quest" (object ["id" .= (1 :: Int)])
      refused `shouldBe` Just (True, "quest #1 has not been taken yet")
      missing <- callTool path "take_quest" (object ["id" .= (1 :: Int)])
      fmap fst missing `shouldBe` Just True
      removeFile path

  describe "answers" $ do
    -- 1人が引き受けられるのは、一度に1つまで
    it "one quest at a time for each adventurer" $
      (applyOpOneEach (Post "Slay slimes" 30) emptyBoard
         >>= applyOpOneEach (Post "Find herbs" 10)
         >>= applyOpOneEach (Take 1 "Lambda")
         >>= applyOpOneEach (Take 2 "Lambda"))
        `shouldBe` Left "Lambda already has a quest"
    -- 道具を1つ足すと、募集中の報酬の合計を答える
    it "total_reward adds up the open quests" $ do
      tmp <- getTemporaryDirectory
      let path = tmp </> "board-mcp-total.json"
      exists <- doesFileExist path
      when exists (removeFile path)
      let call = callWith (totalRewardTool path : boardTools path)
          quest title reward =
            object ["title" .= (title :: Text), "reward" .= (reward :: Int)]
      _ <- call "post_quest" (quest "Slay slimes" 30)
      _ <- call "post_quest" (quest "Find herbs" 10)
      let lambda = object ["id" .= (1 :: Int), "who" .= ("Lambda" :: Text)]
      _ <- call "take_quest" lambda
      total <- call "total_reward" (object [])
      total `shouldBe` Just (False, "10 G")
      removeFile path
