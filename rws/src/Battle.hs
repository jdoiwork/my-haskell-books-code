-- ANCHOR: all
-- ANCHOR: imports
-- | A battle with mtl's State: the hero and a goblin take turns.
-- mtl の State でバトル：勇者とゴブリンが交互に攻撃する
module Battle
  ( Arena (..)
  , start
  , roll
  , heroTurn
  , goblinTurn
  , fight
  ) where

import Control.Monad.State

import Dice (Seed, rollDie)
-- ANCHOR_END: imports

-- ANCHOR: arena
-- 🏟️ Everything that changes during the battle.
-- バトルのあいだに変わるもの、全部
data Arena = Arena
  { heroHp   :: Int
  , goblinHp :: Int
  , seed     :: Seed
  , turns    :: Int
  }
  deriving (Show, Eq)

start :: Seed -> Arena
start s = Arena { heroHp = 30, goblinHp = 25, seed = s, turns = 0 }
-- ANCHOR_END: arena

-- ANCHOR: turns
-- 🎲 Roll a die, keeping the seed inside the arena.
-- サイコロを振る。種はアリーナの中にしまっておく
roll :: State Arena Int
roll = do
  arena <- get
  let (n, next) = rollDie (seed arena)
  put arena { seed = next }
  pure n

-- ⚔️ The hero hits for 2 dice.
-- 勇者は、サイコロ2つ分のダメージ
heroTurn :: State Arena ()
heroTurn = do
  a <- roll
  b <- roll
  modify (\arena -> arena { goblinHp = goblinHp arena - (a + b) })

-- 👺 The goblin hits for 1 die + 1.
-- ゴブリンは、サイコロ1つ ＋ 1
goblinTurn :: State Arena ()
goblinTurn = do
  a <- roll
  modify (\arena -> arena { heroHp = heroHp arena - (a + 1) })

-- 🔁 Take turns until someone falls; answer the winner.
-- だれかが倒れるまで交互に。勝ったほうを答える
fight :: State Arena String
fight = do
  modify (\arena -> arena { turns = turns arena + 1 })
  heroTurn
  goblin <- gets goblinHp
  if goblin <= 0
    then pure "hero"
    else do
      goblinTurn
      hero <- gets heroHp
      if hero <= 0 then pure "goblin" else fight
-- ANCHOR_END: turns
-- ANCHOR_END: all
