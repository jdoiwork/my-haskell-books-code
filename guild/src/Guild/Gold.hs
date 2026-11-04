-- ANCHOR: all
-- ANCHOR: module
-- 💰 Gold: an amount of money that is never negative.
--    Only the type Gold is exported, not its constructor,
--    so the only way to make Gold from outside is mkGold.
-- ゴールド：マイナスにならないお金
-- 型の Gold だけを公開し、コンストラクターは公開しない
-- だから外からゴールドを作る方法は mkGold だけになる
module Guild.Gold
  ( Gold
  , mkGold
  , noGold
  , goldAmount
  , addGold
  ) where
-- ANCHOR_END: module

-- ANCHOR: gold
newtype Gold = Gold Int
  deriving (Eq, Ord)

-- 💬 Show gold as "30 G".
-- ゴールドは "30 G" のように表示する
instance Show Gold where
  show (Gold n) = show n ++ " G"

-- 🛡️ The only door to make Gold: no negative gold.
-- ゴールドを作る、ただ1つの入り口：マイナスは禁止
mkGold :: Int -> Maybe Gold
mkGold n
  | n >= 0    = Just (Gold n)
  | otherwise = Nothing

-- 🫙 No gold at all.
-- ゴールドなし
noGold :: Gold
noGold = Gold 0

-- 🔢 How much gold, as a number.
-- 何ゴールドか（数として）
goldAmount :: Gold -> Int
goldAmount (Gold n) = n

-- ➕ Put two amounts of gold together.
-- 2つのゴールドを合わせる
addGold :: Gold -> Gold -> Gold
addGold (Gold a) (Gold b) = Gold (a + b)
-- ANCHOR_END: gold
-- ANCHOR_END: all
