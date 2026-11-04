-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}

-- ANCHOR: module
-- 🐚 monlist: parse the options, read the file, print the list.
-- monlist：オプションを解析し、ファイルを読み、一覧を表示する
module Main (main) where

import Options.Applicative (execParser)
import qualified Data.Text.IO as TIO

import Mondex.Core    (Outcome (..), listMonsters)
import Mondex.Monlist (MonlistInput (..), monlistInfo)
import Mondex.Store   (loadBestiary)
-- ANCHOR_END: module

-- ANCHOR: main
-- 🚪 Parse the options, then print the monsters.
-- オプションを解析して、モンスターを表示する
main :: IO ()
main = do
  opts <- execParser monlistInfo
  monsters <- loadBestiary opts.bestiaryFile
  let outcome = listMonsters opts.request monsters
  mapM_ TIO.putStrLn outcome.output
-- ANCHOR_END: main
-- ANCHOR_END: all
