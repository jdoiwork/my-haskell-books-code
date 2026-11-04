-- ANCHOR: all
-- 👋 Ask for a name and say hello.
-- 名前を聞いて、あいさつする
main :: IO ()
main = do
  -- 「名前は？」
  putStrLn "What is your name?"
  name <- getLine
  -- 「こんにちは、〇〇！」
  putStrLn ("Hello, " ++ name ++ "!")
-- ANCHOR_END: all
