-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}
-- replicateM: do the same IO action n times, and collect the results.
-- replicateM：同じ IO を n 回くり返して、結果をリストにする
import Control.Monad (replicateM)

-- mapMaybe: map, and keep only what is inside Just.
-- isNothing: is it Nothing?
-- mapMaybe：map して、Just の中身だけを残す
-- isNothing：Nothing かどうか
import Data.Maybe (isNothing, mapMaybe)

-- The pure core of this program.
-- このプログラムの純粋な核
import Appraisal

-- 🔮 Read N lines of "hero,item,rarity", and print the appraisal.
-- "勇者,アイテム,レア度" の行を N 行読み、鑑定書を表示する
main :: IO ()
main = do
  n <- readLn :: IO Int
  rows <- replicateM n getLine
  let loots   = mapMaybe parseLoot rows
      skipped = filter (\row -> isNothing (parseLoot row)) rows
  putStrLn "== Appraisal =="
  mapM_ putStrLn (reportLines (countByRarity loots))
  case rarest loots of
    Just best ->
      putStrLn ("Rarest: " ++ best.item ++ " (" ++ best.hero ++ ")")
    Nothing   -> putStrLn "Rarest: none"
  mapM_ (\row -> putStrLn ("Skipped: " ++ row)) skipped
-- ANCHOR_END: all
