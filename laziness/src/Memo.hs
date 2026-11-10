-- ANCHOR: all
-- | When lazy is the right choice: a table whose values use each other.
-- 遅延が正解のとき：値どうしが互いを使う表
module Memo
  ( memoFib
  , memoFibStrict
  ) where

-- ANCHOR: imports
import qualified Data.Map.Lazy   as ML
import qualified Data.Map.Strict as MS
-- ANCHOR_END: imports

-- ANCHOR: lazy
-- 🐇 Fibonacci with a table. Each entry is computed only when someone
--    asks for it, and only once. The entries use the table itself.
-- 表を使うフィボナッチ。どのマスも、求められたときに1回だけ計算される。
-- マスの中身が、表そのものを使っている
memoFib :: Int -> Integer
memoFib n = table ML.! n
  where
    table = ML.fromList [ (i, fib i) | i <- [0 .. n] ]
    fib 0 = 0
    fib 1 = 1
    fib i = table ML.! (i - 1) + table ML.! (i - 2)
-- ANCHOR_END: lazy

-- ANCHOR: strict
-- 🧱 The same with Data.Map.Strict: to build the table it must compute
--    every entry first, but the entries need the finished table.
-- Data.Map.Strict で同じことをすると：表を作るには先に全部のマスを計算
-- しないといけないのに、マスの計算には完成した表が要る
memoFibStrict :: Int -> Integer
memoFibStrict n = table MS.! n
  where
    table = MS.fromList [ (i, fib i) | i <- [0 .. n] ]
    fib 0 = 0
    fib 1 = 1
    fib i = table MS.! (i - 1) + table MS.! (i - 2)
-- ANCHOR_END: strict
-- ANCHOR_END: all
