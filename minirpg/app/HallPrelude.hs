-- ANCHOR: all
-- ANCHOR: base1
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- ANCHOR: imports
module Main (main) where

-- Files: not in the book's Prelude (it needs another library).
-- ファイルの有無：本書の Prelude には入れていない（別のライブラリが要るため）
import System.Directory (doesFileExist)

-- The pure part: reading and writing records
-- 純粋な部分：記録を読み書きする
import Records
-- ANCHOR_END: imports
-- ANCHOR_END: base1
-- ANCHOR: statsImports

-- Map functions (the Map type itself is in the book's Prelude)
-- Map の関数（Map の型そのものは本書の Prelude に入っている）
import Data.Map.Strict qualified as Map

-- The pure part: counting the records
-- 純粋な部分：記録を数える
import Stats
-- ANCHOR_END: statsImports
-- ANCHOR: base2

-- ANCHOR: main
-- ANCHOR: mainAll
-- 🚀 Look at the command-line arguments and decide what to do.
-- コマンドラインの引数を見て、何をするか決める
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["add", name, result] -> addRecord (toText name) (toText result)
    ["remove", name]      -> removeRecords (toText name)
    ["show"]              -> showRecords
-- ANCHOR_END: base2
-- ANCHOR: statsCommands
    ["stats"]             -> showStats
    ["stats", "--full"]   -> showFullStats
-- ANCHOR_END: statsCommands
-- ANCHOR: base3
    _                     -> die usage

-- 📖 How to use this program.
-- このプログラムの使い方
usage :: String
-- 「使い方：hall add 名前 won|lost|escaped、hall remove 名前、hall show」
usage =
  "usage: hall add NAME won|lost|escaped\n"
    ++ "       hall remove NAME\n"
    ++ "       hall show"
-- ANCHOR_END: base3
-- ANCHOR: statsUsage
    -- 「hall stats [--full]」
    ++ "\n       hall stats [--full]"
-- ANCHOR_END: statsUsage
-- ANCHOR_END: mainAll
-- ANCHOR_END: main
-- ANCHOR: base4

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
    Nothing -> die ("unknown result: " <> toString result)
    Just o  -> do
      let record = Record { hero = name, outcome = o }
      appendFileText recordsFile (formatRecord record <> "\n")
      -- 「保存しました：〇〇」
      putTextLn ("Saved: " <> formatRecord record)
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
      content <- readFileText recordsFile
      pure (mapMaybe parseRecord (lines content))
-- ANCHOR_END: loadRecords

-- ANCHOR: showRecords
-- 🏆 Show every record in the file, and a summary.
-- ファイルの記録を全部と、まとめを表示する
showRecords :: IO ()
showRecords = do
  records <- loadRecords
  if null records
    -- 「まだ記録はありません」
    then putTextLn "No records yet."
    else do
      mapM_ printRecord records
      putTextLn (summary records)

-- 🖨️ Show one record: "Lambda ... won"
-- 記録を1つ表示する："Lambda ... won"
printRecord :: Record -> IO ()
printRecord record =
  let name = record.hero <> " ..."
  in  putTextLn (name <> " " <> outcomeText record.outcome)
-- ANCHOR_END: showRecords

-- ANCHOR: removeRecords
-- 🧹 Remove every record of this hero, and write the rest back.
-- この勇者の記録を全部取り除き、残りをファイルに書き戻す
removeRecords :: Text -> IO ()
removeRecords name = do
  records <- loadRecords
  let kept    = removeHero name records
      removed = length records - length kept
  writeFileText recordsFile (unlines (map formatRecord kept))
  -- 「〇〇の記録を 〇 件削除しました」
  putTextLn
    ("Removed " <> toText (show removed) <> " record(s) of " <> name)
-- ANCHOR_END: removeRecords
-- ANCHOR_END: base4

-- ANCHOR: showStats
-- 📊 Show the number of battles and wins for each hero.
-- 勇者ごとに、戦った回数と勝った回数を表示する
showStats :: IO ()
showStats = do
  records <- loadRecords
  if null records
    -- 「まだ記録はありません」
    then putTextLn "No records yet."
    else mapM_ printStat (statsByHero records)

-- 🖨️ "Lambda ... 3 battles, 2 won"
printStat :: (Text, Int, Int) -> IO ()
printStat (name, battles, wins) =
  let label  = name <> " ..."
      counts = toText (show battles) <> " battles, "
                 <> toText (show wins) <> " won"
  -- 「〇〇 … 〇回の戦い、〇回の勝ち」
  in  putTextLn (label <> " " <> counts)
-- ANCHOR_END: showStats

-- ANCHOR: showFullStats
-- 📊 Show how many times each outcome happened, for each hero.
-- 勇者ごとに、それぞれの結末の回数を表示する
showFullStats :: IO ()
showFullStats = do
  records <- loadRecords
  if null records
    -- 「まだ記録はありません」
    then putTextLn "No records yet."
    else mapM_ printFullStat (Map.toList (countOutcomes records))

-- 🖨️ "Lambda ... won 2, lost 0, escaped 1"
printFullStat :: (Text, OutcomeCounts) -> IO ()
printFullStat (name, counts) =
  let label = name <> " ..."
  -- 「〇〇 … 勝ち 〇、負け 〇、逃げ 〇」
  in  putTextLn (label <> " " <> formatCounts counts)
-- ANCHOR_END: showFullStats
-- ANCHOR_END: all
