module RpgAnswers where

-- randomRIO: get a random number from the outside world.
-- randomRIO：外の世界から乱数をもらう
import System.Random (randomRIO)

import Arena

-- ANCHOR: greetElement
-- 🔥 Ask for a name and a favorite element, then answer.
-- 名前と好きな属性を聞いて、返事をする
greetElement :: IO ()
greetElement = do
  -- 「名前は？」
  putStrLn "What is your name?"
  name <- getLine
  -- 「好きな属性は？」
  putStrLn "What is your favorite element?"
  element <- getLine
  -- 「〇〇は〇〇が好き。いいね！」
  putStrLn (name ++ " likes " ++ element ++ ". Nice!")
-- ANCHOR_END: greetElement

-- ANCHOR: rollTwoDice
-- 🎲 Roll two dice and add them up.
-- サイコロを2つ振って、合計する
rollTwoDice :: IO Int
rollTwoDice = do
  first  <- randomRIO (1, 6)
  second <- randomRIO (1, 6)
  pure (first + second)
-- ANCHOR_END: rollTwoDice

-- ANCHOR: countdown
-- ⏳ Count down to the battle: 3, 2, 1, Fight!
-- 戦闘開始までのカウントダウン：3、2、1、Fight!
countdown :: Int -> IO ()
countdown n
  -- 「戦え！」
  | n <= 0    = putStrLn "Fight!"
  | otherwise = do
      putStrLn (show n ++ "...")
      countdown (n - 1)
-- ANCHOR_END: countdown

-- ANCHOR: askCommand
-- 🔁 Keep asking until the player types a command we know.
-- 知っているコマンドが入力されるまで、聞き続ける
askCommand :: IO Command
askCommand = do
  -- 「コマンドは？」
  putStrLn "Command?"
  input <- getLine
  case parseCommand input of
    Just command -> pure command
    Nothing -> do
      -- 「え？ もう一度どうぞ」
      putStrLn "Huh? Try again."
      askCommand
-- ANCHOR_END: askCommand
