-- ANCHOR: greet
-- 👋 Ask for the hero's name and say hello.
-- 勇者の名前を聞いて、あいさつする
main :: IO ()
main = do
  -- 「勇者よ、名前は？」
  putStrLn "Hero, what is your name?"
  name <- getLine
  -- 「ようこそ、〇〇！」
  let greeting = "Welcome, " ++ name ++ "!"
  putStrLn greeting
-- ANCHOR_END: greet
