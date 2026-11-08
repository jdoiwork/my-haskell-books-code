-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}

-- 📒 lootbook: the IO shell around the pure core.
-- lootbook：純粋な核を包む、IO の殻
module Main (main) where

import Control.Exception    (IOException, try)
import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text            qualified as T
import Options.Applicative  (execParser)
import System.Exit          (die)

import Loot.Book
import Loot.Cli
import Loot.Gold

main :: IO ()
main = do
  opts   <- execParser optionsInfo
  -- Read it all strictly inside try, so that every read error is caught.
  -- try の中で全部を読みきる（読む途中のエラーも、ここで捕まえる）
  loaded <- try (BS.readFile opts.bookFile)
  bytes  <- case loaded of
    -- 「目録のファイルを読めません」
    Left (_ :: IOException) -> die ("lootbook: cannot read " ++ opts.bookFile)
    Right b                 -> pure (BL.fromStrict b)
  items <- case parseBook bytes of
    Left err -> die ("lootbook: " ++ show err)
    Right is -> pure is
  case opts.request of
    -- 「合計 ... ゴールド」
    Total -> do
      let gold = goldAmount (totalValue items)
      putStrLn ("Total: " ++ show gold ++ " G")
    Top n -> mapM_ (putStrLn . T.unpack . (.name)) (topItems n items)
-- ANCHOR_END: all
