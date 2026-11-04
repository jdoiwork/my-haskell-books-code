-- ANCHOR: all
-- | The type chart of elements, the one program this chapter writes
--   in many languages.
-- 属性の相性表。この章で、いろいろな言語で書いてみるプログラム
module Modern.Elements
  ( Element (..)
  , beats
  , power
  ) where

-- ANCHOR: elements
-- 🔥 Three elements, each strong against one other.
-- 3つの属性。それぞれ、1つの属性に強い
data Element = Fire | Water | Grass
  deriving (Show, Eq, Enum, Bounded)

-- ⚔️ Does the attacker's element beat the defender's?
-- 攻める側の属性は、守る側の属性に強い？
beats :: Element -> Element -> Bool
beats Fire  Grass = True
beats Water Fire  = True
beats Grass Water = True
beats _     _     = False

-- 💥 Attack power in percent: 200 when strong, 50 when weak.
-- 攻撃の倍率（パーセント）：強ければ 200、弱ければ 50
power :: Element -> Element -> Int
power attacker defender
  | beats attacker defender = 200
  | beats defender attacker = 50
  | otherwise               = 100
-- ANCHOR_END: elements
-- ANCHOR_END: all
