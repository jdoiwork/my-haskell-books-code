-- ANCHOR: all
-- | State, written by hand: a function from the old state to a result
--   and the new state.
-- 手で書く State：古い状態から、結果と新しい状態を作る関数
module MyState
  ( State (..)
  , evalState
  , execState
  , get
  , put
  , modify
  , rollDieS
  , threeRollsS
  ) where

import Dice

-- ANCHOR: state
-- 📦 A step that reads the state and leaves a new one behind.
-- 状態を読んで、新しい状態を残していく手順
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State step) = State $ \s ->
    let (a, s') = step s
    in  (f a, s')

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  State stepF <*> State stepA = State $ \s ->
    let (f, s1) = stepF s
        (a, s2) = stepA s1
    in  (f a, s2)

-- 🔗 The heart: pass the new state on to the next step.
-- ここが心臓部：新しい状態を、次の手順に渡す
instance Monad (State s) where
  State step >>= next = State $ \s ->
    let (a, s1) = step s
    in  runState (next a) s1
-- ANCHOR_END: state

-- ANCHOR: tools
-- 🧰 Small tools, all built from the same idea.
-- 小さな道具。どれも同じ考え方から作れる
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put new = State $ \_ -> ((), new)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)
-- ANCHOR_END: tools

-- ANCHOR: rolls
-- 🎲 rollDie already has the right shape: Seed -> (Int, Seed).
-- rollDie は、もともとぴったりの形：Seed -> (Int, Seed)
rollDieS :: State Seed Int
rollDieS = State rollDie

-- 🎲🎲🎲 Three rolls: nobody passes the seed by hand any more.
-- 3回振る：もう、だれも種を手で渡さない
threeRollsS :: State Seed [Int]
threeRollsS = do
  a <- rollDieS
  b <- rollDieS
  c <- rollDieS
  pure [a, b, c]
-- ANCHOR_END: rolls
-- ANCHOR_END: all
