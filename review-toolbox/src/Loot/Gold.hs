-- ANCHOR: all
-- 🪙 Gold that is never negative.
--    Only the type is exported, not the constructor:
--    the only way to make Gold from outside is mkGold.
-- マイナスにならないゴールド。型だけを公開し、コンストラクターは隠す。
-- 外からゴールドを作る方法は mkGold だけ
module Loot.Gold
  ( Gold
  , mkGold
  , goldAmount
  , timesGold
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))

newtype Gold = Gold Int
  deriving (Show, Eq, Ord)

mkGold :: Int -> Maybe Gold
mkGold n
  | n >= 0    = Just (Gold n)
  | otherwise = Nothing

goldAmount :: Gold -> Int
goldAmount (Gold n) = n

-- ✖️ The price of n of the same item (n is not negative).
-- 同じ品 n 個ぶんの値段（n はマイナスではない）
timesGold :: Int -> Gold -> Gold
timesGold n (Gold g) = Gold (max 0 n * g)

-- ➕ Gold adds up: (<>) is +, and mempty is 0.
-- ゴールドは足し合わせられる：(<>) は +、mempty は 0
instance Semigroup Gold where
  Gold a <> Gold b = Gold (a + b)

instance Monoid Gold where
  mempty = Gold 0

-- 📥 JSON goes through the same door, mkGold.
-- JSON からも、同じ門 mkGold を通る
instance FromJSON Gold where
  parseJSON v = do
    n <- parseJSON v
    maybe (fail ("negative gold: " ++ show n)) pure (mkGold n)

instance ToJSON Gold where
  toJSON = toJSON . goldAmount
-- ANCHOR_END: all
