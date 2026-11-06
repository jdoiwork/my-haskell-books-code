-- ANCHOR: all
-- | Running State, Reader and Writer from main: run turns them back
--   into plain values, so let and where are all you need.
-- main から State・Reader・Writer を動かす：run でふつうの値に戻すので、
-- let と where があれば十分
module Main (main) where

-- ANCHOR: main
-- The run functions of mtl
-- mtl の run の関数
import Control.Monad          (forM_)
import Control.Monad.Reader   (runReader)
import Control.Monad.State    (runState)
import Control.Monad.Writer.CPS (runWriter)

-- The steps of this chapter
-- この章の手順
import Battle (Arena (..), fight, start)
import Config (easy, floorReport)
import Dice   (Seed)
import Logged (twoAttacks)

-- 🚀 Run each kind of step, and print plain values.
-- それぞれの手順を動かして、ふつうの値を表示する
main :: IO ()
main = do
  -- 1. State with let: a plain pair comes back
  -- 1. State を let で：ふつうの組が返ってくる
  let (winner, arena) = runState fight (start 42)
  putStrLn ("Winner: " ++ winner)
  putStrLn ("Turns: " ++ show (turns arena))

  -- 2. Reader with let: a plain String comes back
  -- 2. Reader を let で：ふつうの String が返ってくる
  let report = runReader (floorReport 3) easy
  putStrLn report

  -- 3. Writer with where (see below)
  -- 3. Writer を where で（下を見る）
  forM_ battleLog $ \line ->
    putStrLn ("log: " ++ line)
  putStrLn ("Goblin HP: " ++ show hpLeft)

  -- 4. A pure function that runs State inside
  -- 4. 中で State を動かす、純粋な関数
  forM_ [1, 2, 3] $ \s ->
    putStrLn (battleSummary s)
  where
    (hpLeft, battleLog) = runWriter (twoAttacks 30)

-- 🏆 Run the battle and describe it in one line.
-- バトルを動かして、1行で説明する
battleSummary :: Seed -> String
battleSummary s =
  "seed " ++ show s ++ ": " ++ winner
    ++ " wins in " ++ show (turns arena) ++ " turns"
  where
    (winner, arena) = runState fight (start s)
-- ANCHOR_END: main
-- ANCHOR_END: all
