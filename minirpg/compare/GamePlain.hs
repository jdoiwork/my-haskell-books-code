{-# LANGUAGE OverloadedRecordDot #-}
-- The whole mini RPG in one file, without comments, to compare with the
-- Python version in the chapter. (The real one is split into Arena.hs
-- and Main.hs.)
-- 章の Python 版と比べるための、コメントなしでミニ RPG を1つのファイルに
-- まとめたもの（本物は Arena.hs と Main.hs に分かれている）
module Main (main) where

import System.IO     (hFlush, stdout)
import System.Random (randomRIO)

-- ANCHOR: body
data Command = Attack | Heal | Run

data Action = Hit Int | Cure Int | Escape

data Battle = Battle { heroHp :: Int, monsterHp :: Int }

-- The pure part: rules and messages / 純粋な部分：ルールと文章
parseCommand :: String -> Maybe Command
parseCommand input
  | input `elem` ["attack", "a"] = Just Attack
  | input `elem` ["heal", "h"]   = Just Heal
  | input `elem` ["run", "r"]    = Just Run
  | otherwise                    = Nothing

heroTurn :: Action -> Battle -> Battle
heroTurn (Hit damage) battle =
  battle { monsterHp = max 0 (battle.monsterHp - damage) }
heroTurn (Cure amount) battle =
  battle { heroHp = min 50 (battle.heroHp + amount) }
heroTurn Escape battle = battle

monsterTurn :: Int -> Battle -> Battle
monsterTurn bite battle =
  battle { heroHp = max 0 (battle.heroHp - bite) }

status :: Battle -> String
status battle =
  "Hero HP: " ++ show battle.heroHp
    ++ "  ⚔️  Slime HP: " ++ show battle.monsterHp

-- The IO part: input, output and random numbers
-- IO の部分：入力・表示・乱数
main :: IO ()
main = battleLoop (Battle { heroHp = 50, monsterHp = 40 })

battleLoop :: Battle -> IO ()
battleLoop battle = do
  putStrLn (status battle)
  putStr "Command (attack / heal / run)> "
  hFlush stdout
  input <- getLine
  case parseCommand input of
    Nothing -> do
      putStrLn "Huh? Try again."
      battleLoop battle
    Just command -> do
      action <- createAction command
      heroActs action battle

createAction :: Command -> IO Action
createAction Attack = do
  damage <- randomRIO (8, 14)
  pure (Hit damage)
createAction Heal = do
  amount <- randomRIO (5, 10)
  pure (Cure amount)
createAction Run = pure Escape

heroActs :: Action -> Battle -> IO ()
heroActs Escape _ = putStrLn "You ran away safely."
heroActs action battle = do
  let afterHero = heroTurn action battle
  if afterHero.monsterHp == 0
    then putStrLn "The Slime is defeated. You win!"
    else monsterAttacks afterHero

monsterAttacks :: Battle -> IO ()
monsterAttacks battle = do
  bite <- randomRIO (3, 9)
  let afterMonster = monsterTurn bite battle
  if afterMonster.heroHp == 0
    then putStrLn "You were defeated... Game over."
    else battleLoop afterMonster
-- ANCHOR_END: body
