-- ANCHOR: head
module Main (main) where

import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck

import FakeAnswers
import FakeIO (FakeIO, World (..), newWorld, runFakeIO)
import Programs
import Screen
-- ANCHOR_END: head

-- ANCHOR: helpers
-- 🧪 Run a program in a new world.
-- 新しい世界で、プログラムを動かす
runIn :: [(String, String)] -> [String] -> FakeIO a -> (a, World)
runIn vars typed program = runFakeIO program (newWorld vars typed)

-- 🧹 The rows without the padding at the end.
-- 行の終わりの空白を取った、画面の行
shownRows :: World -> [String]
shownRows world =
  map (reverse . dropWhile (== ' ') . reverse) (rows (screen world))
-- ANCHOR_END: helpers

-- ANCHOR: sameSize
-- 📐 After writing the text, the screen is still 8 rows of 40 columns.
-- 文字を書いたあとも、画面は 8 行、どの行も 40 文字
sameSize :: String -> Bool
sameSize text =
  let screenRows = rows (writeString text blankScreen)
  in  length screenRows == screenHeight
        && all ((== screenWidth) . length) screenRows
-- ANCHOR_END: sameSize

-- ANCHOR: main
main :: IO ()
main = hspec $ do
-- ANCHOR_END: main
  -- ANCHOR: greetTest
  describe "programs in the fake world" $ do
    -- あいさつして、依頼をたずね、打った行は消費される
    it "greet" $ do
      let (_, world) = runIn [("USER", "Lambda")] ["slay the slime"] greet
      take 3 (shownRows world) `shouldBe`
        [ "Welcome, Lambda!"
        , "Which quest? slay the slime"
        , "Good luck with slay the slime."
        ]
      stdinLines world `shouldBe` []
    -- ANCHOR_END: greetTest
    -- 環境変数がなければ stranger
    it "greet without USER" $ do
      let (_, world) = runIn [] ["x"] greet
      take 1 (shownRows world) `shouldBe` ["Welcome, stranger!"]
    -- あふれた行は流れて消える
    it "countdown scrolls" $ do
      let (_, world) = runIn [] [] (countdown 12)
      shownRows world `shouldBe`
        ["count 7", "count 6", "count 5", "count 4", "count 3"
        , "count 2", "count 1", ""]
    -- 長い行は折り返す
    it "a long line wraps" $ do
      let (_, world) = runIn [] [] longLine
      take 4 (shownRows world) `shouldBe`
        replicate 3 "slime goblin dragon slime goblin dragon" <> [""]
    -- 環境変数は変わらない
    it "the environment never changes" $ do
      let (_, world) = runIn [("USER", "Lambda")] ["a"] greet
      environment world `shouldBe` Map.fromList [("USER", "Lambda")]
  -- ANCHOR: screenProps
  describe "the screen" $ do
    -- 何を書いても、画面は 8 行、どの行も 40 文字
    it "always has the same size" $ property sameSize
    -- 改行の多い文字でも。画面があふれて、上の行が流れていく
    it "keeps the size when it scrolls" $
      forAll (listOf (elements "ab\n")) $ \text ->
        classify (length (lines text) > screenHeight) "scrolled"
          (sameSize text)
  -- ANCHOR_END: screenProps
  describe "answers" $ do
    -- 数が来るまでたずねる
    it "askNumber" $
      fst (runIn [] ["ten", "10"] askNumber) `shouldBe` 10
    -- 入力の行を数える
    it "countLines" $
      fst (runIn [] ["a", "b", "c"] countLines) `shouldBe` 3
