{-# LANGUAGE OverloadedRecordDot #-}
-- ANCHOR: module
-- The rules of the battle. Everything here is pure:
-- no input, no output, no random numbers.
-- バトルのルール。ここはすべて純粋：入出力も乱数もない
module Arena where
-- ANCHOR_END: module

-- ANCHOR: command
-- ⚔️ What the hero can do in one turn.
-- 勇者が1ターンにできること
data Command = Attack | Heal | Run
  deriving (Show, Eq)

-- 🔤 Read a command typed by the player. One letter is also OK.
-- プレイヤーが入力したコマンドを読む。1文字の省略形でもよい
parseCommand :: String -> Maybe Command
parseCommand input =
  case input of
    "attack" -> Just Attack
    "a"      -> Just Attack
    "heal"   -> Just Heal
    "h"      -> Just Heal
    "run"    -> Just Run
    "r"      -> Just Run
    _        -> Nothing
-- ANCHOR_END: command

-- ANCHOR: action
-- 🎲 What actually happens, after the dice are rolled.
--    A Command is what the player typed; an Action also knows
--    the numbers. Running away needs no numbers.
-- サイコロを振ったあとの、実際に起きること
-- Command はプレイヤーが打ち込んだもの、Action は数まで決まったもの
-- 逃げるのに数は要らない
data Action
  = Hit Int   -- hit the monster for this much / これだけダメージを与える
  | Cure Int  -- heal this much HP / これだけ HP を回復する
  | Escape    -- run away / 逃げる
  deriving (Show, Eq)
-- ANCHOR_END: action

-- ANCHOR: battle
-- 🏟️ The state of a battle: the HP of both sides.
-- バトルの状態：両者の HP
data Battle = Battle
  { heroHp    :: Int
  , monsterHp :: Int
  }
  deriving (Show, Eq)

-- 🏁 How every battle starts.
-- バトルの始まりの状態
startBattle :: Battle
startBattle = Battle { heroHp = 50, monsterHp = 40 }
-- ANCHOR_END: battle

-- ANCHOR: turns
-- 💥 The hero acts. The random numbers are already inside the Action,
--    so this function stays pure.
-- 勇者の行動。乱数はもう Action の中に入っているので、この関数は純粋なまま
heroTurn :: Action -> Battle -> Battle
heroTurn (Hit damage) battle =
  battle { monsterHp = max 0 (battle.monsterHp - damage) }
heroTurn (Cure amount) battle =
  battle { heroHp = min 50 (battle.heroHp + amount) }
heroTurn Escape battle = battle

-- 👾 The monster bites back.
-- モンスターの反撃
monsterTurn :: Int -> Battle -> Battle
monsterTurn bite battle =
  battle { heroHp = max 0 (battle.heroHp - bite) }
-- ANCHOR_END: turns

-- ANCHOR: messages
-- 📊 One line that shows the HP of both sides.
-- 両者の HP を表示する1行
status :: Battle -> String
status battle =
  -- 「勇者の HP：〇〇  ⚔️  スライムの HP：〇〇」
  "Hero HP: " ++ show battle.heroHp
    ++ "  ⚔️  Slime HP: " ++ show battle.monsterHp

-- 💬 What happened in the hero's turn.
-- 勇者のターンに起きたこと
heroMessage :: Action -> String
-- 「スライムに 〇〇 のダメージを与えた！」
heroMessage (Hit damage)  = "You hit the Slime for " ++ show damage ++ "!"
-- 「HP が 〇〇 回復した」
heroMessage (Cure amount) = "You heal " ++ show amount ++ " HP."
-- 「うまく逃げ切った」
heroMessage Escape        = "You ran away safely."
-- ANCHOR_END: messages
