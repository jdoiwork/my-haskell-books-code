-- ANCHOR: module
module Guild where

-- readMaybe: read a string as a value, or Nothing if it fails.
-- readMaybe：文字列を値として読む（読めなければ Nothing）
import Text.Read (readMaybe)
-- ANCHOR_END: module

-- ANCHOR: job
-- 🧙 The jobs a hero can choose at the guild.
-- ギルドで選べる職業
data Job = Warrior | Mage | Thief | Healer
  deriving (Show, Eq, Ord, Enum, Bounded, Read)
-- ANCHOR_END: job

-- ANCHOR: allJobs
-- 📋 Every job, from the first to the last.
-- 最初から最後まで、すべての職業
allJobs :: [Job]
allJobs = [minBound .. maxBound]
-- ANCHOR_END: allJobs

-- ANCHOR: nextJob
-- 🔁 The next job on the menu. After the last one, back to the first.
-- メニューの次の職業。最後の次は、最初に戻る
nextJob :: Job -> Job
nextJob job
  | job == maxBound = minBound
  | otherwise       = succ job
-- ANCHOR_END: nextJob

-- ANCHOR: parseJob
-- 🔤 Read a job name typed by the player.
-- プレイヤーが入力した職業名を読む
parseJob :: String -> Maybe Job
parseJob input = readMaybe input
-- ANCHOR_END: parseJob

-- ANCHOR: gold
-- 🪙 Gold that can be added like a number.
--    Num is borrowed from the Int inside.
-- 数のように足せるゴールド。Num は中身の Int から借りてくる
newtype Gold = Gold Int
  deriving (Show, Eq, Ord, Num)
-- ANCHOR_END: gold
