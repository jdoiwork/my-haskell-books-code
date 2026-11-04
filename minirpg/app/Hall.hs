-- Let us write Text values as "..." literals.
-- "..." の書き方で Text の値を書けるようにする
{-# LANGUAGE OverloadedStrings #-}
-- Let us write record.hero to read a field.
-- record.hero のようにフィールドを読めるようにする
{-# LANGUAGE OverloadedRecordDot #-}

-- ANCHOR: imports
module Main (main) where

-- 📦 Tools for the outside world. Yes, a lot of imports again!
-- 外の世界とやりとりする道具。そう、また import がたくさん！

-- Text and its input and output
-- Text とその入出力
import Data.Text    (Text)
import Data.Text    qualified as T
import Data.Text.IO qualified as TIO

-- Keep only the values that are Just
-- Just の値だけを残す
import Data.Maybe (mapMaybe)

-- Files, command-line arguments, and stopping with an error
-- ファイル、コマンドラインの引数、エラーで止める
import System.Directory   (doesFileExist)
import System.Environment (getArgs)
import System.Exit        (die)

-- The pure part: reading and writing records
-- 純粋な部分：記録を読み書きする
import Records
-- ANCHOR_END: imports

-- ANCHOR: main
-- 🚀 Look at the command-line arguments and decide what to do.
-- コマンドラインの引数を見て、何をするか決める
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["add", name, result] -> addRecord (T.pack name) (T.pack result)
    ["remove", name]      -> removeRecords (T.pack name)
    ["show"]              -> showRecords
    _                     -> die usage

-- 📖 How to use this program.
-- このプログラムの使い方
usage :: String
-- 「使い方：hall add 名前 won|lost|escaped、hall remove 名前、hall show」
usage =
  "usage: hall add NAME won|lost|escaped\n"
    ++ "       hall remove NAME\n"
    ++ "       hall show"
-- ANCHOR_END: main

-- ANCHOR: recordsFile
-- 📁 The file where the hall of fame is kept.
-- 殿堂の記録を置いておくファイル
recordsFile :: FilePath
recordsFile = "hall-of-fame.txt"
-- ANCHOR_END: recordsFile

-- ANCHOR: addRecord
-- ✍️ Add one record to the end of the file.
-- ファイルの最後に、記録を1行足す
addRecord :: Text -> Text -> IO ()
addRecord name result =
  case parseOutcome result of
    -- 「知らない結末：〇〇」
    Nothing -> die ("unknown result: " <> T.unpack result)
    Just o  -> do
      let record = Record { hero = name, outcome = o }
      TIO.appendFile recordsFile (formatRecord record <> "\n")
      -- 「保存しました：〇〇」
      TIO.putStrLn ("Saved: " <> formatRecord record)
-- ANCHOR_END: addRecord

-- ANCHOR: loadRecords
-- 📂 Read every record in the file. No file yet: no records.
-- ファイルの記録を全部読む。ファイルがまだなければ、記録なし
loadRecords :: IO [Record]
loadRecords = do
  exists <- doesFileExist recordsFile
  if not exists
    then pure []
    else do
      content <- TIO.readFile recordsFile
      pure (mapMaybe parseRecord (T.lines content))
-- ANCHOR_END: loadRecords

-- ANCHOR: showRecords
-- 🏆 Show every record in the file, and a summary.
-- ファイルの記録を全部と、まとめを表示する
showRecords :: IO ()
showRecords = do
  records <- loadRecords
  if null records
    -- 「まだ記録はありません」
    then TIO.putStrLn "No records yet."
    else do
      mapM_ printRecord records
      TIO.putStrLn (summary records)

-- 🖨️ Show one record: "Lambda ... won"
-- 記録を1つ表示する："Lambda ... won"
printRecord :: Record -> IO ()
printRecord record =
  let name = T.justifyLeft 12 '.' record.hero -- "Lambda......"
  in  TIO.putStrLn (name <> " " <> outcomeText record.outcome)
-- ANCHOR_END: showRecords

-- ANCHOR: removeRecords
-- 🧹 Remove every record of this hero, and write the rest back.
-- この勇者の記録を全部取り除き、残りをファイルに書き戻す
removeRecords :: Text -> IO ()
removeRecords name = do
  records <- loadRecords
  let kept    = removeHero name records
      removed = length records - length kept
  TIO.writeFile recordsFile (T.unlines (map formatRecord kept))
  -- 「〇〇の記録を 〇 件削除しました」
  TIO.putStrLn
    ("Removed " <> T.pack (show removed) <> " record(s) of " <> name)
-- ANCHOR_END: removeRecords
