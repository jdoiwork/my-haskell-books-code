-- ANCHOR: all
-- ✖️ Read two numbers on one line, and print their product.
-- 1行に並んだ2つの数を読み、積を表示する
main :: IO ()
main = do
  line <- getLine
  case map read (words line) :: [Int] of
    [a, b] -> print (a * b)
    -- 「2つの数が必要です」
    _      -> putStrLn "expected two numbers"
-- ANCHOR_END: all
