{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
module Main (main) where

import Control.Exception
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import System.Exit
import System.IO

import Vault

-- ANCHOR: main
-- 🌃 close-books LEDGER DEPOSITS: add the day's deposits to the ledger.
--    The known accidents are caught here, at the very outside.
-- close-books 帳簿 預け入れ：1日の預け入れを帳簿に足す
-- 知っている事故は、いちばん外側のここで受け止める
main :: IO ()
main = do
  args <- getArgs
  case args of
    [ledger, deposits] ->
      closeBooks ledger deposits
        `catches` [Handler fileTrouble, Handler vaultTrouble]
    _ -> die "usage: close-books LEDGER DEPOSITS"

-- 📂 A file we could not read or write: exit code 1.
-- 読めない・書けないファイル：終了コード 1
fileTrouble :: IOException -> IO ()
fileTrouble e = do
  hPutStrLn stderr ("close-books: " <> displayException e)
  exitWith (ExitFailure 1)

-- 🚨 An accident in the vault: exit code 2.
-- 宝物庫の事故：終了コード 2
vaultTrouble :: VaultAccident -> IO ()
vaultTrouble e = do
  hPutStrLn stderr ("close-books: the books do not close: " <> show e)
  exitWith (ExitFailure 2)
-- ANCHOR_END: main

-- ANCHOR: close
-- 📒 Read, add up, save, and write one line in the journal.
-- 読んで、足して、保存して、日誌に1行書く
closeBooks :: FilePath -> FilePath -> IO ()
closeBooks ledger depositsPath = do
  entries <- loadLedger ledger
  text    <- TIO.readFile depositsPath
  let merged  = addUp (entries ++ parseLedger text)
      total   = sum [ e.gold | e <- merged ]
      summary = "closed: " <> T.pack (show (length merged)) <> " owners, "
                <> T.pack (show total) <> " gold"
  saveLedger ledger merged
  withJournal (ledger <> ".journal") (\ h -> TIO.hPutStrLn h summary)
  TIO.putStrLn summary

-- ➕ Add up the gold of each owner.
-- 持ち主ごとに、金貨を足し合わせる
addUp :: [Entry] -> [Entry]
addUp entries =
  [ Entry who g
  | (who, g) <- Map.toList (Map.fromListWith (+) pairs)
  ]
  where
    pairs = [ (e.owner, e.gold) | e <- entries ]
-- ANCHOR_END: close
-- ANCHOR_END: all
