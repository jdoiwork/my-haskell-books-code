-- ANCHOR: all
-- ANCHOR: imports
-- digitToInt: turn a digit character into a number ('7' -> 7).
-- digitToInt：数字の文字を数にする（'7' → 7）
import Data.Char (digitToInt)

-- (&): pass a value to a function, left to right (x & f == f x).
-- (&)：値を関数に左から右へ流す（x & f は f x と同じ）
import Data.Function ((&))
-- ANCHOR_END: imports

-- ANCHOR: digitSum
-- 🔢 The sum of the digits: 14 -> "14" -> [1, 4] -> 5
-- 各桁の和：14 → "14" → [1, 4] → 5
digitSum :: Int -> Int
digitSum number =
  show number
    & map digitToInt
    & sum
-- ANCHOR_END: digitSum

-- ANCHOR: treasure
-- 💰 Boxes 1 to n. Only a box whose digits add up to k is real,
--    and it holds as much gold as its number. Add up that gold.
-- 1 番から n 番の宝箱。各桁の和が k の箱だけが本物で、
-- 番号と同じだけのゴールドが入っている。その合計を出す
treasure :: Int -> Int -> Int
treasure n k =
  [1 .. n]
    & filter (\box -> digitSum box == k)
    & sum
-- ANCHOR_END: treasure

-- ANCHOR: main
-- 🗝️ Problem 5: read N and K on one line, and print the total gold.
-- 問題5：1行の N と K を読み、ゴールドの合計を表示する
main :: IO ()
main = do
  line <- getLine
  case map read (words line) :: [Int] of
    [n, k] -> print (treasure n k)
    -- "two numbers are needed"
    -- 「2つの数が必要です」
    _      -> putStrLn "expected two numbers"
-- ANCHOR_END: main
-- ANCHOR_END: all
