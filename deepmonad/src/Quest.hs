-- ANCHOR: all
-- | The quest from "Monad: an introduction", again: a journey that
--   either fails somewhere or clears with a result.
-- 「Monad 入門」の依頼を、もう一度：どこかで失敗するか、結果を持って進む旅
module Quest
  ( Quest (..)
  , step
  , forest
  , cave
  , tower
  , journey
  , journeyK
  , lowerFloors
  ) where

-- ANCHOR: imports
import Control.Monad ((>=>))
-- ANCHOR_END: imports

-- ANCHOR: type
-- 🗺️ A quest fails at a place, or clears with a result.
-- 依頼は、ある場所で失敗するか、結果を持って進む
data Quest a
  = Failed String
  | Cleared a
  deriving (Show, Eq)

instance Functor Quest where
  fmap _ (Failed place) = Failed place
  fmap f (Cleared x)    = Cleared (f x)

instance Applicative Quest where
  pure = Cleared
  Failed place <*> _ = Failed place
  Cleared f    <*> q = fmap f q

instance Monad Quest where
  Failed place >>= _    = Failed place
  Cleared x    >>= next = next x
-- ANCHOR_END: type

-- ANCHOR: steps
-- ⚔️ One step: lose some HP; fail if it runs out.
-- 1歩：HP が減る。尽きたら失敗
step :: String -> Int -> Int -> Quest Int
step place damage hp
  | hp > damage = Cleared (hp - damage)
  | otherwise   = Failed place

forest, cave, tower :: Int -> Quest Int
forest = step "forest" 30
cave   = step "cave"   40
tower  = step "tower"  50

-- 🗺️ With do, as in the introduction.
-- 入門の章と同じ、do で
journey :: Int -> Quest Int
journey hp = do
  hp1 <- forest hp
  hp2 <- cave hp1
  tower hp2
-- ANCHOR_END: steps

-- ANCHOR: kleisli
-- 🐟 The same journey, with the fish operator >=> joining the steps.
-- 同じ旅を、魚の演算子 >=> で手順をつないで
journeyK :: Int -> Quest Int
journeyK = forest >=> cave >=> tower

-- 🏚️ Thanks to the associative law, a part can get its own name.
-- 結合法則のおかげで、途中までに名前を付けられる
lowerFloors :: Int -> Quest Int
lowerFloors = forest >=> cave
-- ANCHOR_END: kleisli
-- ANCHOR_END: all
