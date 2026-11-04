-- ANCHOR: all
-- 📏 Ways to add up a battle log, and how much memory each one keeps.
-- バトルログを足し合わせる、いろいろな書き方。どれだけメモリを抱えるか
module Main (main) where

import Data.Text          qualified as T
import Control.Exception  (evaluate)
import System.Environment (getArgs)
import System.Mem         (performGC)

-- ANCHOR: damages
-- ⚔️ Ten million damages, from 0 to 96.
-- 1000万回のダメージ（0〜96）
damages :: [Int]
damages = [ n `mod` 97 | n <- [1 .. 10000000] ]
-- ANCHOR_END: damages

-- ANCHOR: sumTo
-- 🔁 Tail recursion: the running total is carried along.
-- 末尾再帰：途中の合計を、引数で持ち運ぶ
sumTo :: Int -> [Int] -> Int
sumTo acc []       = acc
sumTo acc (x : xs) = sumTo (acc + x) xs

-- 🔁 The same, but the total is worked out at each step (!).
-- 同じものを、毎回その場で合計を計算しながら（!）
sumToBang :: Int -> [Int] -> Int
sumToBang !acc []       = acc
sumToBang !acc (x : xs) = sumToBang (acc + x) xs
-- ANCHOR_END: sumTo

-- ANCHOR: tally
-- 📊 How many hits, and how much damage in all.
-- 攻撃の回数と、ダメージの合計
data Tally = Tally
  { hits  :: Int
  , total :: Int
  }
  deriving (Show)

-- 📊 The same, with strict fields: worked out when the Tally is made.
-- 同じものを、正格なフィールドで（Tally を作るときに計算する）
data StrictTally = StrictTally
  { strictHits  :: !Int
  , strictTotal :: !Int
  }
  deriving (Show)

addHit :: Tally -> Int -> Tally
addHit t d = Tally { hits = hits t + 1, total = total t + d }

addHitStrict :: StrictTally -> Int -> StrictTally
addHitStrict t d =
  StrictTally
    { strictHits  = strictHits t + 1
    , strictTotal = strictTotal t + d
    }
-- ANCHOR_END: tally

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["foldl"]       -> print (foldl  (+) 0 damages)
    ["foldl'"]      -> print (foldl' (+) 0 damages)
    ["foldr"]       -> print (foldr  (+) 0 damages)
    ["sumTo"]       -> print (sumTo 0 damages)
    ["sumToBang"]   -> print (sumToBang 0 damages)
    ["tally"]       -> print (foldl' addHit (Tally 0 0) damages)
    ["strictTally"] -> print (foldl' addHitStrict (StrictTally 0 0) damages)
    ["string"]      -> keepString
    ["text"]        -> keepText
    _               -> putStrLn "usage: measure WHICH +RTS -s"

-- ANCHOR: keep
-- 📝 Keep a million characters in memory, as a String and as a Text.
-- 100万文字を、String と Text でメモリに置いておく
keepString :: IO ()
keepString = do
  s <- evaluate (forceString (concat (replicate 250000 "冒険者よ")))
  performGC
  print (length s)
  where
    forceString x = length x `seq` x

keepText :: IO ()
keepText = do
  t <- evaluate (T.replicate 250000 (T.pack "冒険者よ"))
  performGC
  print (T.length t)
-- ANCHOR_END: keep
-- ANCHOR_END: all
