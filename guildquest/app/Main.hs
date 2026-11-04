-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- ANCHOR: imports
-- 📋 questboard: the guild's quest board on the command line.
-- questboard：ギルドの依頼板を、コマンドラインから
module Main (main) where

-- SQLite.
-- SQLite
import Database.SQLite.Simple (Connection, withConnection)

-- Text, reading numbers, and stopping with an error.
-- テキスト、数の読み取り、エラーで止める
import Data.Maybe         (fromMaybe)
import Data.Text          (Text)
import Data.Text          qualified as T
import Data.Text.IO       qualified as TIO
import System.Environment (getArgs)
import System.Exit        (die)
import Text.Read          (readMaybe)

-- The guild: its database, rules, and shape.
-- ギルドのデータベース、規則、形
import Guild.Quest
import Guild.Schema (migrate)
import Guild.Store
-- ANCHOR_END: imports

-- ANCHOR: main
-- 🚀 Open the database, grow it to the newest version, then run.
-- データベースを開き、いちばん新しい版まで育ててから、コマンドを動かす
main :: IO ()
main = do
  args <- getArgs
  withConnection "guild.db" $ \conn -> do
    _ <- migrate conn
    run conn args

-- 🧭 Do what the arguments say.
-- 引数のとおりに動く
run :: Connection -> [String] -> IO ()
run conn args =
  case args of
    ["join", who, jobName]  -> joinGuild conn (T.pack who) (T.pack jobName)
    ["post", what, gold]    -> withNumber gold (postQuest conn (T.pack what))
    ["take", qid, who]      -> withNumber qid (takeOne conn (T.pack who))
    ["done", qid]           -> withNumber qid (complete conn)
    ["board"]               -> questBoard conn >>= mapM_ printLine
    ["members"]             -> members conn >>= mapM_ printMember
    _                       -> die usage

-- 📖 How to use this program.
-- このプログラムの使い方
usage :: String
usage =
  "usage: questboard join NAME JOB\n"
    ++ "       questboard post TITLE REWARD\n"
    ++ "       questboard take QUEST NAME\n"
    ++ "       questboard done QUEST\n"
    ++ "       questboard board\n"
    ++ "       questboard members"
-- ANCHOR_END: main

-- ANCHOR: commands
-- 🔢 Read a number, then go on with it.
-- 数を読んでから、先へ進む
withNumber :: String -> (Int -> IO ()) -> IO ()
withNumber word next =
  case readMaybe word of
    -- 「数で書いてね：〇〇」
    Nothing -> die ("not a number: " ++ word)
    Just n  -> next n

-- 🧙 questboard join NAME JOB
joinGuild :: Connection -> Text -> Text -> IO ()
joinGuild conn who jobName = do
  addMember conn who jobName
  -- 「〇〇がギルドに入った」
  TIO.putStrLn (who <> " joined the guild")

-- 📌 questboard post TITLE REWARD
postQuest :: Connection -> Text -> Int -> IO ()
postQuest conn what gold = do
  qid <- post conn what gold
  -- 「依頼〇を貼った：〇〇」
  TIO.putStrLn ("posted quest " <> T.pack (show qid) <> ": " <> what)

-- ✋ questboard take QUEST NAME
takeOne :: Connection -> Text -> Int -> IO ()
takeOne conn who qid = do
  ok <- takeQuest conn qid who
  if ok
    -- 「〇〇が依頼〇を受けた」
    then TIO.putStrLn (who <> " took quest " <> T.pack (show qid))
    -- 「受けられない：〇」
    else die ("cannot take quest " ++ show qid)

-- 🏁 questboard done QUEST
complete :: Connection -> Int -> IO ()
complete conn qid = do
  result <- completeQuest conn qid
  case result of
    -- 「依頼〇は完了。〇ゴールドを払った」
    Right gold -> putStrLn ("quest " ++ show qid ++ " done, paid "
                              ++ show gold ++ " gold")
    Left err   -> die (explain err)

-- 💬 A QuestError, in words.
-- QuestError を、文にする
explain :: QuestError -> String
explain err =
  case err of
    -- 「依頼〇はない」「誰も受けていない」「もう完了している」
    NoSuchQuest qid -> "there is no quest " ++ show qid
    NotTaken qid    -> "no one has taken quest " ++ show qid
    AlreadyDone qid -> "quest " ++ show qid ++ " is already done"

-- 🖨️ "1  Slay the slime   50 G  Lambda  done"
printLine :: BoardLine -> IO ()
printLine l =
  TIO.putStrLn $ T.intercalate "  "
    [ T.justifyLeft 2 ' ' (T.pack (show l.lineId))
    , T.justifyLeft 18 ' ' l.lineTitle
    , T.justifyRight 4 ' ' (T.pack (show l.lineReward)) <> " G"
    , T.justifyLeft 7 ' ' (fromMaybe "-" l.taker)
    , if l.lineDone then "done" else "open"
    ]

-- 🖨️ "Lambda      Mage      120 G"
printMember :: Member -> IO ()
printMember m =
  TIO.putStrLn $
    T.justifyLeft 12 ' ' m.memberName
      <> T.justifyLeft 10 ' ' m.memberJob
      <> T.pack (show m.gold) <> " G"
-- ANCHOR_END: commands
-- ANCHOR_END: all
