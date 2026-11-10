-- ANCHOR: all
-- ANCHOR: imports
-- | Answers to the training of the fake IO chapter.
-- IO もどきの章のトレーニングの解答
module FakeAnswers
  ( askNumber
  , countLines
  ) where

import Prelude hiding (getLine, putStr, putStrLn)

import Text.Read (readMaybe)

import Console
import Programs (putStrLn)
-- ANCHOR_END: imports

-- ANCHOR: askNumber
-- 🔢 Step 2: ask again until a number comes.
-- ステップ2：数が来るまで、何度でもたずねる
askNumber :: Console m => m Int
askNumber = do
  -- 「レベルは？」
  putStr "Level? "
  answer <- getLine
  case readMaybe answer of
    Just n  -> pure n
    Nothing -> do
      -- 「数で答えてください」
      putStrLn "Please answer with a number."
      askNumber
-- ANCHOR_END: askNumber

-- ANCHOR: countLines
-- 📏 Step 3: count the lines until the input runs out.
-- ステップ3：入力が尽きるまで、行を数える
countLines :: Console m => m Int
countLines = go 0
  where
    go :: Console m => Int -> m Int
    go n = do
      done <- isEOF
      if done
        then pure n
        else do
          _ <- getLine
          go (n + 1)
-- ANCHOR_END: countLines
-- ANCHOR_END: all
