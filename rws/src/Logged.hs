-- ANCHOR: all
-- | Keeping a log: by hand with pairs, and with Writer.
-- 記録をためる：組を使って手で、そして Writer で
module Logged
  ( attackByHand
  , twoAttacksByHand
  , attack
  , twoAttacks
  ) where

-- ANCHOR: imports
import Control.Monad.Writer.CPS
-- ANCHOR_END: imports

-- ANCHOR: byHand
-- 📝 By hand: return the result and the log, and join the logs.
-- 手で：結果と記録を返し、記録どうしをつなぐ
attackByHand :: Int -> Int -> ([String], Int)
attackByHand damage hp =
  (["hit for " ++ show damage], hp - damage)

twoAttacksByHand :: Int -> ([String], Int)
twoAttacksByHand hp0 =
  let (log1, hp1) = attackByHand 7 hp0
      (log2, hp2) = attackByHand 9 hp1
  in  (log1 ++ log2, hp2)
-- ANCHOR_END: byHand

-- ANCHOR: writer
-- ✍️ With Writer: tell adds to the log; joining is automatic.
-- Writer で：tell で記録に足す。つなぐのは自動
attack :: Int -> Int -> Writer [String] Int
attack damage hp = do
  tell ["hit for " ++ show damage]
  pure (hp - damage)

twoAttacks :: Int -> Writer [String] Int
twoAttacks hp0 = do
  hp1 <- attack 7 hp0
  attack 9 hp1
-- ANCHOR_END: writer
-- ANCHOR_END: all
