-- ANCHOR: module
module Bestiary where
-- ANCHOR_END: module

-- ANCHOR: element
-- 🔥 The elements. A value of this type is one of these three.
-- 属性。この型の値は、この3つのどれか
data Element = Fire | Water | Grass
  deriving (Show, Eq)
-- ANCHOR_END: element

-- ANCHOR: elementBonus
-- 🔥 Bonus (in percent) by the attacker's element
--    and the defender's element.
-- 攻撃側の属性と防御側の属性による倍率（パーセント）
elementBonus :: Element -> Element -> Int
elementBonus Fire  Grass = 150 -- fire beats grass / 炎は草に強い
elementBonus Water Fire  = 150 -- water beats fire / 水は炎に強い
elementBonus Grass Water = 150 -- grass beats water / 草は水に強い
elementBonus Grass Fire  = 50  -- the reverse is weak / 逆は弱い
elementBonus Fire  Water = 50
elementBonus Water Grass = 50
elementBonus _     _     = 100 -- anything else: normal / それ以外は等倍
-- ANCHOR_END: elementBonus

-- ANCHOR: strongAgainst
-- 🎯 Which element does this element beat?
-- この属性は、どの属性に強い？
strongAgainst :: Element -> Element
strongAgainst Fire  = Grass
strongAgainst Water = Fire
strongAgainst Grass = Water
-- ANCHOR_END: strongAgainst

-- ANCHOR: action
-- ⚔️ What a hero can do in one turn.
--    Some choices carry a value with them.
-- 勇者が1ターンにできること
-- 値を持っている選択肢もある
data Action
  = Attack Int    -- attack with this power / この威力で攻撃
  | Spell Element -- cast a spell of this element / この属性の呪文
  | Heal Int      -- heal this much HP / これだけ HP を回復
  | Defend        -- defend (no value) / 防御（値なし）
  deriving (Show, Eq)
-- ANCHOR_END: action

-- ANCHOR: describe
-- 💬 A message for the battle log.
-- バトルログに出すメッセージ
describe :: Action -> String
describe (Attack power) = "Attack with power " ++ show power
describe (Spell e)      = "Cast a " ++ show e ++ " spell"
describe (Heal amount)  = "Heal " ++ show amount ++ " HP"
describe Defend         = "Defend"
-- ANCHOR_END: describe

-- ANCHOR: either
-- 🛒 The shop again. This time a failure comes with a reason.
--    Left: why it failed. Right: the gold left.
-- もう一度、道具屋。今度は失敗に理由が付く
-- Left：失敗した理由。Right：残りのお金
price :: String -> Maybe Int
price item =
  case item of
    "potion" -> Just 50
    "elixir" -> Just 150
    _        -> Nothing

buy :: String -> Int -> Either String Int
buy item gold =
  case price item of
    Nothing -> Left (item ++ " is not for sale")
    Just p
      | p <= gold -> Right (gold - p)
      | otherwise -> Left ("not enough gold for " ++ item)
-- ANCHOR_END: either
