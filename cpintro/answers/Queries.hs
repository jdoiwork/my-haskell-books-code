-- ANCHOR: all
-- replicateM_: do the same IO action n times (no results needed).
-- replicateM_：同じ IO を n 回くり返す（結果は要らない）
import Control.Monad (replicateM_)

-- ❓ Read T, then T lines of "a b". For each line, print a + b.
-- T を読み、"a b" の行を T 行読む。行ごとに a + b を表示する
main :: IO ()
main = do
  t <- readLn :: IO Int
  replicateM_ t answerOne

-- ➕ Answer one query.
-- 問い合わせ1つに答える
answerOne :: IO ()
answerOne = do
  line <- getLine
  let numbers = map read (words line) :: [Int]
  print (sum numbers)
-- ANCHOR_END: all
