-- ANCHOR: all
-- 🏋️ Answers for the training.
-- トレーニングの解答
module MonadAnswers (totalPrice, totalPriceAp, affordableSets) where

import Control.Monad (guard)

import Shop (price)

-- ANCHOR: total
-- 💰 The price of two items together, with do.
-- 2つのアイテムの値段の合計（do で）
totalPrice :: String -> String -> Maybe Int
totalPrice a b = do
  pa <- price a
  pb <- price b
  pure (pa + pb)

-- 💰 The same, with <$> and <*>: the two prices do not depend on
--    each other, so Applicative is enough.
-- 同じものを <$> と <*> で：2つの値段は頼り合わないので、
-- Applicative で足りる
totalPriceAp :: String -> String -> Maybe Int
totalPriceAp a b = (+) <$> price a <*> price b
-- ANCHOR_END: total

-- ANCHOR: sets
-- 🛍️ Every pair of a weapon and a potion we can afford with the gold.
-- この所持金で買える、武器と薬の組をすべて
affordableSets :: Int -> [(String, String)]
affordableSets gold = do
  weapon <- ["sword", "bow", "axe"]
  potion <- ["potion", "elixir"]
  guard (cost weapon + cost potion <= gold)
  pure (weapon, potion)
  where
    cost item = case item of
      "sword"  -> 300
      "bow"    -> 200
      "axe"    -> 250
      "potion" -> 50
      _        -> 150
-- ANCHOR_END: sets
-- ANCHOR_END: all
