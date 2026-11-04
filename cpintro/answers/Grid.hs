-- ANCHOR: all
-- replicateM: do the same IO action n times.
-- replicateM：同じ IO を n 回くり返す
import Control.Monad (replicateM)

-- 🧱 Read H and W, then H rows of '#' and '.'.
--    Print how many '#' are in each row, separated by spaces.
-- H と W を読み、'#' と '.' でできた H 行を読む
-- 各行の '#' の数を、空白で区切って表示する
main :: IO ()
main = do
  -- "3 4": the number before the space is H
  -- "3 4"：空白の前の数が H
  sizeLine <- getLine
  let h = read (takeWhile (/= ' ') sizeLine) :: Int
  rows <- replicateM h getLine
  let counts = map (\row -> length (filter (== '#') row)) rows
  putStrLn (unwords (map show counts))
-- ANCHOR_END: all
