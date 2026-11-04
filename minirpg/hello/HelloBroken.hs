-- ANCHOR: all
-- 👋 Ask for a name and say hello. (This does NOT compile.)
-- 名前を聞いて、あいさつする（これはコンパイルできない）
main :: IO ()
main = do
  -- 「名前は？」
  putStrLn "What is your name?"
  -- 「こんにちは、〇〇！」
  putStrLn ("Hello, " ++ getLine ++ "!")
-- ANCHOR_END: all
