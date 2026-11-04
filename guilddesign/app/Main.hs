-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}

-- 🏰 The guild command. GUILD_STORE chooses where the roster lives.
-- ギルドのコマンド。名簿の置き場は GUILD_STORE で選ぶ
module Main (main) where

import Data.Text          qualified as T
import Data.Text.IO       qualified as T
import Data.Maybe         (fromMaybe)
import System.Environment (getArgs, lookupEnv)
import System.Exit        (die)
import Text.Read          (readMaybe)

import Guild.Roster
import Guild.Store
import Guild.Config
import Guild.Service

-- ANCHOR: main
main :: IO ()
main = do
  setting <- fromMaybe "sqlite:guild.db" <$> lookupEnv "GUILD_STORE"
  config  <- either die pure (parseStoreConfig setting)
  args    <- getArgs
  withStore config (\store -> run store args)
-- ANCHOR_END: main

-- ANCHOR: run
-- 🏃 Do what the arguments say, on any store.
-- 引数のとおりに動く（どの置き場でも）
run :: RosterStore -> [String] -> IO ()
run store ["add", who, job, lv]
  | Just n <- readMaybe lv =
      enlist store (NewAdventurer (T.pack who) (T.pack job) n) >>= T.putStrLn
run store ["list"] =
  rosterReport store >>= mapM_ T.putStrLn
run store ["levelup", who] = do
  done <- store.levelUp (T.pack who)
  -- 「レベルアップ！」／「その名前の冒険者はいません」
  putStrLn (if done then "Level up!" else "No such adventurer.")
run _ _ =
  -- 「使い方：guild add 名前 職業 レベル | list | levelup 名前」
  die "usage: guild add NAME JOB LEVEL | list | levelup NAME"
-- ANCHOR_END: run
-- ANCHOR_END: all
