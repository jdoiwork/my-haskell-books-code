-- ANCHOR: all
-- | Passing settings around: by hand, and with Reader.
-- 設定を持ち回る：手で、そして Reader で
module Config
  ( Config (..)
  , easy
  , floorReportByHand
  , greeting
  , enemyHp
  , floorReport
  , bossFloor
  ) where

-- ANCHOR: imports
import Control.Monad.Reader
-- ANCHOR_END: imports

-- ANCHOR: config
-- ⚙️ Settings that many functions need to read.
-- たくさんの関数が読みたい設定
data Config = Config
  { playerName :: String
  , difficulty :: Int
  }
  deriving (Show, Eq)

easy :: Config
easy = Config { playerName = "Lambda", difficulty = 1 }
-- ANCHOR_END: config

-- ANCHOR: byHand
-- 🪣 By hand: every function takes the config, just to pass it on.
-- 手で：どの関数も、次に渡すためだけに、設定を受け取る
greetingByHand :: Config -> String
greetingByHand cfg = "Welcome, " ++ playerName cfg ++ "!"

enemyHpByHand :: Config -> Int -> Int
enemyHpByHand cfg base = base * difficulty cfg

floorReportByHand :: Config -> Int -> String
floorReportByHand cfg floorNo =
  greetingByHand cfg ++ " Floor " ++ show floorNo
    ++ ": goblin with " ++ show (enemyHpByHand cfg (floorNo * 10)) ++ " HP"
-- ANCHOR_END: byHand

-- ANCHOR: reader
-- 📖 With Reader: the config is just "there"; ask for it when needed.
-- Reader で：設定は「そこにある」。要るときに尋ねる
greeting :: Reader Config String
greeting = do
  name <- asks playerName
  pure ("Welcome, " ++ name ++ "!")

enemyHp :: Int -> Reader Config Int
enemyHp base = do
  level <- asks difficulty
  pure (base * level)

floorReport :: Int -> Reader Config String
floorReport floorNo = do
  hello <- greeting
  hp    <- enemyHp (floorNo * 10)
  pure (hello ++ " Floor " ++ show floorNo
          ++ ": goblin with " ++ show hp ++ " HP")
-- ANCHOR_END: reader

-- ANCHOR: local
-- 👹 The boss floor: twice as hard, only inside this part.
-- ボスの階：この部分の中だけ、難しさが2倍
bossFloor :: Reader Config String
bossFloor =
  local (\cfg -> cfg { difficulty = difficulty cfg * 2 }) (floorReport 10)
-- ANCHOR_END: local
-- ANCHOR_END: all
