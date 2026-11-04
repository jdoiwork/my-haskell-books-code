-- ANCHOR: all
-- 👋 IO's do is the same (>>=) chain.
-- IO の do も、同じ (>>=) のつながり
module Greet (greetDo, greetBind) where

-- ANCHOR: greet
-- 👋 Ask a name, then greet (with do).
-- 名前を聞いて、あいさつする（do で）
greetDo :: IO ()
greetDo = do
  -- 「名前は？」
  putStrLn "Your name?"
  who <- getLine
  putStrLn ("Hello, " ++ who ++ "!")

-- 👋 The same, written with (>>=) and (>>).
-- 同じものを、(>>=) と (>>) で
greetBind :: IO ()
greetBind =
  putStrLn "Your name?" >>
  getLine >>= \who ->
  putStrLn ("Hello, " ++ who ++ "!")
-- ANCHOR_END: greet
-- ANCHOR_END: all
