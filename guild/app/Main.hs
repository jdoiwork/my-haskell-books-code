-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Maybe (isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

import Guild.Board (boardLines, totalReward)
import Guild.Quest (Rank (..), mkQuest)

-- 📝 Quests as the clerk wrote them. One reward is a typo.
-- 受付係が書いた依頼。1つ、報酬の書き間違いがある
rawQuests :: [(Text, Rank, Int)]
rawQuests =
  [ ("Pick herbs",          Novice,  30)
  , ("Catch the slime",     Novice,  50)
  , ("Guard the caravan",   Veteran, 200)
  , ("Slay the dragon",     Hero,    -5)
  , ("Find the lost sword", Hero,    1000)
  ]

-- 📋 Show the board, the total reward, and the rejected quests.
-- 依頼板と報酬の合計、断った依頼を表示する
main :: IO ()
main = do
  let toQuest (t, r, g) = mkQuest t r g
      quests   = mapMaybe toQuest rawQuests
      rejected = [ t | raw@(t, _, _) <- rawQuests, isNothing (toQuest raw) ]
  TIO.putStrLn "=== Guild Quest Board ==="
  mapM_ TIO.putStrLn (boardLines quests)
  TIO.putStrLn ("Total reward: " <> T.pack (show (totalReward quests)))
  mapM_ (\t -> TIO.putStrLn ("Rejected: " <> t)) rejected
-- ANCHOR_END: all
