-- ANCHOR: imports
module Main (main) where

-- hFlush and stdout: show the prompt right away.
-- hFlush と stdout：入力をうながす文字をすぐに表示する
import System.IO (hFlush, stdout)

-- randomRIO: get a random number from the outside world.
-- randomRIO：外の世界から乱数をもらう
import System.Random (randomRIO)

-- The pure rules of the battle.
-- バトルの純粋なルール
import Arena
-- ANCHOR_END: imports

-- ANCHOR: main
-- 🚀 Start the game: ask the hero's name, then fight!
-- ゲームを始める：勇者の名前を聞いてから、戦闘開始！
main :: IO ()
main = do
  -- 「野生のスライムが現れた！」
  putStrLn "A wild Slime appeared!"
  -- 「勇者よ、名前は？」
  putStr "Hero, what is your name? "
  hFlush stdout
  name <- getLine
  -- 「行け、〇〇！」
  putStrLn ("Go, " ++ name ++ "!")
  battleLoop startBattle
-- ANCHOR_END: main

-- ANCHOR: battleLoop
-- 🔁 One turn of the battle: show the status, read a command,
--    turn it into an action, and let the hero act.
-- バトルの1ターン：状態を表示し、コマンドを読み、
-- 行動（Action）にして、勇者を動かす
battleLoop :: Battle -> IO ()
battleLoop battle = do
  -- an empty line, to make each turn easy to see
  -- ターンの区切りが見やすいように、空行を1つ
  putStrLn ""
  putStrLn (status battle)
  -- 「コマンド（攻撃 / 回復 / 逃げる）＞」
  putStr "Command (attack / heal / run)> "
  hFlush stdout
  input <- getLine
  case parseCommand input of
    Nothing      -> askAgain battle
    Just command -> do
      action <- createAction command
      heroActs action battle

-- ❓ An unknown command: say so, and ask again with the same battle.
-- 知らないコマンド：そう伝えて、同じ状態でもう一度聞く
askAgain :: Battle -> IO ()
askAgain battle = do
  -- 「え？ もう一度どうぞ」
  putStrLn "Huh? Try again."
  battleLoop battle

-- 🎲 Turn a command into an action: roll the dice if needed.
-- コマンドを行動にする：必要ならサイコロを振る
createAction :: Command -> IO Action
createAction Attack = do
  damage <- randomRIO (8, 14)
  pure (Hit damage)
createAction Heal = do
  amount <- randomRIO (5, 10)
  pure (Cure amount)
createAction Run = pure Escape

-- ⚔️ The hero acts. Then the monster's turn, victory, or the end.
-- 勇者が行動する。そのあとモンスターのターン、勝利、またはおしまい
heroActs :: Action -> Battle -> IO ()
heroActs Escape _ = putStrLn (heroMessage Escape)
heroActs action battle = do
  putStrLn (heroMessage action)
  let afterHero = heroTurn action battle
  if monsterHp afterHero == 0
    -- 「スライムを倒した。あなたの勝ち！」
    then putStrLn "The Slime is defeated. You win!"
    else monsterAttacks afterHero

-- 👾 The monster's turn. Then the next turn, or game over.
-- モンスターのターン。そのあと次のターンへ、またはゲームオーバー
monsterAttacks :: Battle -> IO ()
monsterAttacks battle = do
  bite <- randomRIO (3, 9)
  -- 「スライムが噛みついてきた！ 〇〇のダメージ！」
  putStrLn ("The Slime bites you for " ++ show bite ++ "!")
  let afterMonster = monsterTurn bite battle
  if heroHp afterMonster == 0
    -- 「やられてしまった……ゲームオーバー」
    then putStrLn "You were defeated... Game over."
    else battleLoop afterMonster
-- ANCHOR_END: battleLoop
