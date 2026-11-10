-- ANCHOR: all
-- ANCHOR: imports
-- | Programs written once, for any Console.
-- 一度だけ書いて、どの Console でも動くプログラム
module Programs
  ( putStrLn
  , greet
  , countdown
  , longLine
  ) where

import Prelude hiding (getLine, putStr, putStrLn)

import Data.Maybe (fromMaybe)

import Console

-- 📝 putStrLn is putStr with a newline at the end.
-- putStrLn は、最後に改行を足した putStr
putStrLn :: Console m => String -> m ()
putStrLn text = putStr (text <> "\n")
-- ANCHOR_END: imports

-- ANCHOR: greet
-- 👋 Greet the user named in $USER, and ask for their quest.
-- $USER の人にあいさつして、依頼をたずねる
greet :: Console m => m ()
greet = do
  name <- lookupEnv "USER"
  -- 「ようこそ、○○さん！」
  putStrLn ("Welcome, " <> fromMaybe "stranger" name <> "!")
  -- 「どの依頼を受けますか？」
  putStr "Which quest? "
  quest <- getLine
  -- 「○○、がんばって！」
  putStrLn ("Good luck with " <> quest <> ".")
-- ANCHOR_END: greet

-- ANCHOR: countdown
-- 🔟 Count down from n, one number per row.
-- n から数え下げる。1行に1つ
countdown :: Console m => Int -> m ()
countdown n = mapM_ (\k -> putStrLn ("count " <> show k)) [n, n - 1 .. 1]

-- 📜 A line longer than the screen is wide.
-- 画面の横幅より長い1行
longLine :: Console m => m ()
longLine =
  putStrLn (concat (replicate 6 "slime goblin dragon "))
-- ANCHOR_END: countdown
-- ANCHOR_END: all
