-- ANCHOR: module
module Battle where
-- ANCHOR_END: module

-- ANCHOR: rawDamage
-- 🗡️ Damage = attack - defense. (Simple, but something is wrong...)
-- ダメージ = 攻撃力 - 防御力（シンプルだけど、何かがおかしい……）
rawDamage :: Int -> Int -> Int
rawDamage attack defense = attack - defense
-- ANCHOR_END: rawDamage

-- ANCHOR: damage
-- ⚔️ Damage is at least 1, even against a very hard enemy.
-- ダメージは、どんなに硬い敵が相手でも最低 1 にする
damage :: Int -> Int -> Int
damage attack defense
  | raw < 1   = 1
  | otherwise = raw
  where
    raw = attack - defense
-- ANCHOR_END: damage

-- ANCHOR: hitMessage
-- 📣 A message that matches the damage dealt.
-- 与えたダメージに合わせたメッセージ
hitMessage :: Int -> String
hitMessage d
  -- 「やりすぎた！」
  | d >= 100  = "Overkill!"
  -- 「大ダメージ！」
  | d >= 50   = "Great hit!"
  -- 「いい一撃」
  | d >= 10   = "Good hit."
  -- 「かすり傷……」
  | otherwise = "Just a scratch..."
-- ANCHOR_END: hitMessage

-- ANCHOR: damagePrime
-- ⚔️ The same as damage, but reusing rawDamage.
-- damage と同じ。ただし rawDamage を再利用する
damage' :: Int -> Int -> Int
damage' attack defense
  | raw < 1   = 1
  | otherwise = raw
  where
    raw = rawDamage attack defense
-- ANCHOR_END: damagePrime

-- ANCHOR: status
-- 💬 Describe the state of a character from its HP.
-- HP からキャラクターの状態を説明する
status :: Int -> String
status hp = if hp <= 0 then "Defeated!" else "Still standing"
-- ANCHOR_END: status

-- ANCHOR: elementBonus
-- 🔥 Bonus (in percent) by the attacker's element and the defender's element.
-- 攻撃側の属性と防御側の属性による倍率（パーセント）
elementBonus :: String -> String -> Int
elementBonus "fire"  "grass" = 150 -- fire beats grass / 炎は草に強い
elementBonus "water" "fire"  = 150 -- water beats fire / 水は炎に強い
elementBonus "grass" "water" = 150 -- grass beats water / 草は水に強い
elementBonus "grass" "fire"  = 50  -- the reverse is weak / 逆は弱い
elementBonus "fire"  "water" = 50
elementBonus "water" "grass" = 50
elementBonus _       _       = 100 -- anything else: normal / それ以外は等倍
-- ANCHOR_END: elementBonus

-- ANCHOR: elementalDamage
-- 🌈 Damage with the element bonus applied.
-- 属性の倍率をかけたダメージ
elementalDamage :: String -> String -> Int -> Int -> Int
elementalDamage attackerElement defenderElement attack defense =
  let base  = damage attack defense
      bonus = elementBonus attackerElement defenderElement
  in  base * bonus `div` 100
-- ANCHOR_END: elementalDamage

-- ANCHOR: critical
-- 💥 A critical hit doubles the damage.
-- 会心の一撃ならダメージが2倍
critical :: Bool -> Int -> Int
critical True  d = d * 2
critical False d = d
-- ANCHOR_END: critical
