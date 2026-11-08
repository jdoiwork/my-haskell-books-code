-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

-- try: run an IO action, and turn its exception into a Left.
-- try：IO を実行して、例外を Left に変える
import Control.Exception (IOException, try)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import System.Exit (die)

import Registry

-- 📋 register FILE: register every line of the file.
-- register ファイル名：ファイルの各行を登録する
main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> registerFile path
    _      -> die "usage: register FILE"

-- 📂 Read the file. A missing file is not a crash, but a message.
-- ファイルを読む。ファイルがなくても落ちずに、メッセージを出す
registerFile :: FilePath -> IO ()
registerFile path = do
  result <- try (TIO.readFile path) :: IO (Either IOException Text)
  case result of
    Left err       -> die ("cannot read " <> path <> ": " <> show err)
    Right contents -> report (registerAll (T.lines contents))

-- 🖨️ Show who was registered, and why the others were not.
-- 登録された人と、登録できなかった理由を表示する
report :: ([RegistrationError], [Adventurer]) -> IO ()
report (errors, adventurers) = do
  TIO.putStrLn "== Registered =="
  mapM_ (TIO.putStrLn . adventurerLine) adventurers
  TIO.putStrLn "== Rejected =="
  mapM_ (\e -> TIO.putStrLn ("- " <> message e)) errors

-- 🧑 "- Lambda (Mage)"
adventurerLine :: Adventurer -> Text
adventurerLine a = "- " <> a.name <> " (" <> T.pack (show a.job) <> ")"
-- ANCHOR_END: all
