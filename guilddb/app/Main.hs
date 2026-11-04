-- ANCHOR: all
-- ANCHOR: imports
-- 📒 roster: the guild's roster on the command line.
-- roster：ギルドの名簿を、コマンドラインから
module Main (main) where

-- SQLite, and its error type.
-- SQLite と、そのエラーの型
import Database.SQLite.Simple (Connection, SQLError (..), withConnection)

-- Text, reading numbers, and stopping with an error.
-- テキスト、数の読み取り、エラーで止める
import Control.Exception  (try)
import Data.Text          qualified as T
import Data.Text.IO       qualified as TIO
import System.Environment (getArgs)
import System.Exit        (die)
import Text.Read          (readMaybe)

-- The pure part: the roster and its SQL.
-- 名簿と、その SQL
import Roster
-- ANCHOR_END: imports

-- ANCHOR: main
-- 📁 The database file of the guild.
-- ギルドのデータベースのファイル
rosterFile :: FilePath
rosterFile = "guild.db"

-- 🚀 Open the database, make the table if needed, then run the command.
-- データベースを開き、必要なら表を作って、コマンドを動かす
main :: IO ()
main = do
  args <- getArgs
  withConnection rosterFile $ \conn -> do
    createTable conn
    run conn args

-- 🧭 Do what the arguments say.
-- 引数のとおりに動く
run :: Connection -> [String] -> IO ()
run conn args =
  case args of
    ["add", who, jobName, lv] -> add conn who jobName lv
    ["list"]                  -> list conn
    ["levelup", who]          -> changeOne up "leveled up" who
    ["remove", who]           -> changeOne out "left the guild" who
    _                         -> die usage
  where
    up  = levelUp conn
    out = removeAdventurer conn

-- 📖 How to use this program.
-- このプログラムの使い方
usage :: String
-- 「使い方：roster add 名前 職業 レベル、roster list、
--   roster levelup 名前、roster remove 名前」
usage =
  "usage: roster add NAME JOB LEVEL\n"
    ++ "       roster list\n"
    ++ "       roster levelup NAME\n"
    ++ "       roster remove NAME"
-- ANCHOR_END: main

-- ANCHOR: commands
-- ➕ roster add NAME JOB LEVEL
add :: Connection -> String -> String -> String -> IO ()
add conn who jobName lv =
  case readMaybe lv of
    -- 「レベルは数で：〇〇」
    Nothing -> die ("level must be a number: " ++ lv)
    Just n  -> do
      let new = NewAdventurer (T.pack who) (T.pack jobName) n
      result <- try (addAdventurer conn new)
      case result of
        -- 「登録できなかった：〇〇」
        Left err -> die ("could not add: " ++ T.unpack (sqlErrorDetails err))
        -- 「〇〇を登録した」
        Right () -> putStrLn ("added " ++ who)

-- 📋 roster list
list :: Connection -> IO ()
list conn = do
  adventurers <- listAdventurers conn
  mapM_ (TIO.putStrLn . formatAdventurer) adventurers

-- 🔁 roster levelup NAME / roster remove NAME:
--    change one adventurer, or stop if there is no such adventurer.
-- 冒険者1人を変える。その名前の冒険者がいなければ、エラーで止める
changeOne :: (T.Text -> IO Int) -> String -> String -> IO ()
changeOne change done who = do
  changed <- change (T.pack who)
  if changed == 0
    -- 「〇〇は名簿にいない」
    then die (who ++ " is not in the roster")
    -- 「〇〇は（レベルが上がった／ギルドを去った）」
    else putStrLn (who ++ " " ++ done)
-- ANCHOR_END: commands
-- ANCHOR_END: all
