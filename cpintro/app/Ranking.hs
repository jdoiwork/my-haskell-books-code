-- ANCHOR: imports
-- replicateM: do the same IO action n times, and collect the results.
-- replicateM：同じ IO を n 回くり返して、結果をリストにする
import Control.Monad (replicateM)

-- sortOn and Down: sort by a key, largest first.
-- sortOn と Down：キーで並べ替える（大きい順）
import Data.List (sortOn)
import Data.Ord  (Down (..))
-- ANCHOR_END: imports

-- ANCHOR: splitOn
-- ✂️ Split a string at every separator: splitOn ',' "a,b" == ["a","b"]
-- 区切り文字のところで、文字列を分ける
splitOn :: Char -> String -> [String]
splitOn sep text =
  case break (== sep) text of
    (before, [])       -> [before]
    (before, _ : rest) -> before : splitOn sep rest
-- ANCHOR_END: splitOn

-- ANCHOR: parse
-- 🔍 Read one line "Lambda,85" as a pair ("Lambda", 85).
-- 1行 "Lambda,85" を、組 ("Lambda", 85) として読む
parseLine :: String -> (String, Int)
parseLine line =
  case splitOn ',' line of
    [name, score] -> (name, read score)
    _             -> error ("bad line: " ++ line)
-- ANCHOR_END: parse

-- ANCHOR: main
-- 🏆 Problem 2: read N lines of "name,score".
--    Print a ranking: the highest score first, and by name on a tie.
-- 問題2："名前,点数" の行を N 行読み、点数の高い順に並べて表示する
-- 同じ点数なら、名前の順
main :: IO ()
main = do
  n <- readLn :: IO Int
  rows <- replicateM n getLine
  let players = map parseLine rows
      ranked  = sortOn (\(name, score) -> (Down score, name)) players
  mapM_ printRow (zip [1 :: Int ..] ranked)

-- 🖨️ "1. Python (92)"
printRow :: (Int, (String, Int)) -> IO ()
printRow (rank, (name, score)) =
  putStrLn (show rank ++ ". " ++ name ++ " (" ++ show score ++ ")")
-- ANCHOR_END: main
