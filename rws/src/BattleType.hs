-- ANCHOR: all
-- ANCHOR: imports
-- | A name for the type of battle steps.
-- バトルの手順の型に、名前を付ける
module BattleType (Battle, rollTwice) where

import Control.Monad.State (State)

import Battle (Arena, roll)
-- ANCHOR_END: imports

-- ANCHOR: alias
-- ⚔️ A battle step: State over the arena.
--    The last type (the result) is left out.
-- バトルの手順：アリーナの State。最後の型（結果）は書かない
type Battle = State Arena

-- 🎲🎲 Roll two dice and add them.
-- サイコロを2つ振って、足す
rollTwice :: Battle Int
rollTwice = do
  a <- roll
  b <- roll
  pure (a + b)
-- ANCHOR_END: alias
-- ANCHOR_END: all
