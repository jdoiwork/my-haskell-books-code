-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}

-- 🛡️ Try the guild with selda.
-- selda でギルドを試す
module Main (main) where

import Database.Selda
import Database.Selda.SQLite (withSQLite)

import TypedGuild

-- ANCHOR: main
main :: IO ()
main = withSQLite ":memory:" $ do
  createTable adventurers
  createTable quests
  lambda <- addAdventurer "Lambda" "Mage" 3
  _ <- addAdventurer "Python" "Archer" 5
  _ <- addAdventurer "Rust" "Knight" 1
  _ <- levelUp "Lambda"
  _ <- removeAdventurer "Rust"
  _ <- postQuest "Slay the slime" 50 (Just lambda)
  _ <- postQuest "Find the herb" 30 Nothing
  strongOnes <- query (strong 3)
  board      <- query questBoard
  liftIO $ do
    mapM_ print strongOnes
    mapM_ print board
-- ANCHOR_END: main
-- ANCHOR_END: all
