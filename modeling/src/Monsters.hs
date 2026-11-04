-- ANCHOR: pragma
-- Let us write monster.hp to read a field.
-- monster.hp のように書いてフィールドを読めるようにする
{-# LANGUAGE OverloadedRecordDot #-}
-- ANCHOR_END: pragma

-- ANCHOR: module
module Monsters where

-- The elements and their bonus, from the previous chapter.
-- 前の章で作った属性と、その相性
import Bestiary (Element (..), elementBonus)
-- ANCHOR_END: module

-- ANCHOR: monster
-- 👾 A monster in the bestiary. Each field has a name.
-- モンスター図鑑のモンスター。フィールドには名前が付いている
data Monster = Monster
  { name    :: String
  , hp      :: Int
  , element :: Element
  }
  deriving (Show, Eq)
-- ANCHOR_END: monster

-- ANCHOR: monsters
-- 👾 Some monsters.
-- モンスターたち
slime :: Monster
slime = Monster { name = "Slime", hp = 30, element = Water }

goblin :: Monster
goblin = Monster { name = "Goblin", hp = 50, element = Grass }
-- ANCHOR_END: monsters

-- ANCHOR: hitMonster
-- 💥 Hit a monster with attack power atk.
--    Returns a new monster with less HP.
-- 攻撃力 atk でモンスターを殴る。HP が減った新しいモンスターを返す
hitMonster :: Int -> Monster -> Monster
hitMonster atk monster =
  monster { hp = max 0 (monster.hp - atk) }
-- ANCHOR_END: hitMonster

-- ANCHOR: castOn
-- 🪄 Cast a spell of this element on a monster.
--    The damage is 20, changed by the element bonus.
-- この属性の呪文をモンスターにかける
-- ダメージは 20 で、属性の相性で変わる
castOn :: Element -> Monster -> Monster
castOn e monster =
  let damage = 20 * elementBonus e monster.element `div` 100
  in  hitMonster damage monster
-- ANCHOR_END: castOn

-- ANCHOR: newtype
-- 🪙 Gold and HP are both Int inside,
--    but they are different types, so we cannot mix them up.
-- ゴールドも HP も中身は Int だが、別の型なので取り違えない
newtype Gold = Gold Int
  deriving (Show, Eq)

newtype Hp = Hp Int
  deriving (Show, Eq)

-- 🧪 Drink a potion: HP goes up by 30, up to 100.
-- ポーションを飲む：HP が 30 回復する（最大 100）
drinkPotion :: Hp -> Hp
drinkPotion (Hp n) = Hp (min 100 (n + 30))
-- ANCHOR_END: newtype

-- ANCHOR: mkGold
-- 🛡️ The only way to make Gold from outside: no negative gold.
-- 外からゴールドを作るための、ただ1つの入り口：マイナスは禁止
mkGold :: Int -> Maybe Gold
mkGold n
  | n >= 0    = Just (Gold n)
  | otherwise = Nothing
-- ANCHOR_END: mkGold

-- ANCHOR: reward
-- 🎁 A reward has two parts, so it is a data, not a newtype.
-- 報酬は2つの部分を持つので、newtype ではなく data にする
data Reward = Reward
  { rewardGold :: Gold
  , rewardExp  :: Int
  }
  deriving (Show, Eq)
-- ANCHOR_END: reward

-- ANCHOR: questBad
-- 😵 A quest, modeled with two fields. Some combinations make
--    no sense: done is False but there is a reward?
-- 2つのフィールドで表したクエスト
-- おかしな組み合わせが作れてしまう（未完了なのに報酬がある？）
data QuestBad = QuestBad
  { done   :: Bool
  , reward :: Maybe Int
  }
  deriving Show
-- ANCHOR_END: questBad

-- ANCHOR: quest
-- 📜 A quest, modeled so that wrong states cannot be made.
-- おかしな状態を作れないように表したクエスト
data Quest
  = NotStarted       -- not started yet / まだ始めていない
  | InProgress Int   -- how many monsters defeated / 倒した数
  | Completed Gold   -- done, with the reward / 完了（報酬付き）
  deriving (Show, Eq)

-- 📋 The quest board shows each quest like this.
-- クエスト掲示板での表示
questStatus :: Quest -> String
questStatus NotStarted           = "Not started"
questStatus (InProgress n)       = "In progress: " ++ show n ++ " defeated"
questStatus (Completed (Gold g)) = "Completed! Reward: " ++ show g ++ " G"
-- ANCHOR_END: quest
