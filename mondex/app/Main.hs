-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}

-- ANCHOR: module
-- 🐚 The IO shell of mondex: read the arguments and the file,
--    run the pure core, then print and save.
-- mondex の IO の殻：引数とファイルを読み、純粋な核を動かし、表示して保存する
module Main (main) where

-- The command-line parser.
-- コマンドライン引数の解析
import Options.Applicative (execParser)

-- Text, the environment, and stopping with an error.
-- テキスト、環境変数、エラーで止まる
import Data.Maybe         (fromMaybe)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import System.Exit        (die)

import Mondex.Command (MondexInput (..), mondexInfo)
import Mondex.Core    (Outcome (..), runAction)
import Mondex.Store   (loadBestiary, saveBestiary)
-- ANCHOR_END: module

-- ANCHOR: main
-- 🚪 Parse the arguments, run the command, show the result.
-- 引数を解析し、コマンドを実行して、結果を見せる
main :: IO ()
main = do
-- ANCHOR: env
  envFile <- lookupEnv "MONDEX_FILE"
  let defaultFile = fromMaybe "monsters.json" envFile
-- ANCHOR_END: env
  opts <- execParser (mondexInfo defaultFile)
  monsters <- loadBestiary opts.dexFile
  case runAction opts.dexAction monsters of
    -- 「mondex: （失敗した理由）」
    Left err -> die ("mondex: " ++ T.unpack err)
    Right outcome -> do
      mapM_ TIO.putStrLn outcome.output
      case outcome.changed of
        Just newMonsters -> saveBestiary opts.dexFile newMonsters
        Nothing          -> pure ()
-- ANCHOR_END: main
-- ANCHOR_END: all
