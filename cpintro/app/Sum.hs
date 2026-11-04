-- ANCHOR: all
-- 🧮 Problem 1: read N, then N numbers on one line.
--    Print their sum and their maximum, separated by a space.
-- 問題1：N を読み、次の行の N 個の数を読む
-- 合計と最大値を、空白で区切って表示する
main :: IO ()
main = do
  -- line 1: N (we do not need it, because words finds every number)
  -- 1行目：N（words が数を全部見つけてくれるので、使わない）
  _ <- getLine
  -- line 2: "3 1 4 1 5"
  -- 2行目："3 1 4 1 5"
  line <- getLine
  let numbers = map read (words line) :: [Int]
      answer  = show (sum numbers) ++ " " ++ show (maximum numbers)
  putStrLn answer
-- ANCHOR_END: all
