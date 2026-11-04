{-# LANGUAGE OverloadedRecordDot #-}
module MonstersAnswers where

import Monsters

-- ANCHOR: isDefeated
-- 💀 Has this monster been defeated?
-- このモンスターは倒された？
isDefeated :: Monster -> Bool
isDefeated monster = monster.hp == 0
-- ANCHOR_END: isDefeated

-- ANCHOR: renameMonster
-- 🏷️ Give a monster a new name. Everything else stays the same.
-- モンスターに新しい名前を付ける。ほかはそのまま
renameMonster :: String -> Monster -> Monster
renameMonster newName monster = monster { name = newName }
-- ANCHOR_END: renameMonster

-- ANCHOR: progress
-- 📈 Defeat one more monster in a quest.
--    A quest in progress counts up; other quests stay as they are.
-- クエストでモンスターをもう1体倒す
-- 進行中なら数が増え、それ以外はそのまま
progress :: Quest -> Quest
progress (InProgress n) = InProgress (n + 1)
progress quest          = quest
-- ANCHOR_END: progress

-- ANCHOR: level
-- ⬆️ A hero's level. It is an Int inside, but not just any Int.
-- 勇者のレベル。中身は Int だが、ただの Int ではない
newtype Level = Level Int
  deriving (Show, Eq)

-- ⬆️ Go up one level.
-- レベルを1つ上げる
levelUp :: Level -> Level
levelUp (Level n) = Level (n + 1)
-- ANCHOR_END: level
