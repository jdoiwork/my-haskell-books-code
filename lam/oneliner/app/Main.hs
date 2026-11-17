-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | lam: the REPL, run a file, or run a one-liner.
--     lam [flags]  /  lam [flags] program.lam args...
--     lam [flags] -e 'program' args...  /  lam -ne '...' files...
--   The flags (--allow-net=pokeapi.co, --trace, -n, ...) come before
--   the file.
-- lam：REPL、ファイルを動かす、またはワンライナーを動かす。フラグ
-- （--allow-net、--trace、-n など）はファイルより前に。ファイルの後ろは、
-- スクリプトの引数
module Main (main) where

import Control.Exception (NonTermination (..), evaluate, try)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.IO.Utf8 as Utf8
import System.Console.Haskeline
import System.Environment (getArgs, withArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)

import Lam.Guard (guarded)
import Lam.Io (Handler)
import Lam.Module (importAt, loadProgram, runProgram)
import Lam.OneLiner (Command (..), OneLiner (..), readCommand, runOneLiner)
import Lam.Script (runScriptFlag)
import Lam.Prelude (preludeSession)
import Lam.Program (Ending (..), Session)
import Lam.Repl (Input (..), endingText, parseInput, step)
import qualified Lam.Program as Program
import Lam.World (realFiles, realWorld, withOutside)

main :: IO ()
main = do
  args <- getArgs
  case readCommand args of
    Left err -> do
      -- 「使い方：lam [フラグ] [ファイル 引数…]」
      TIO.hPutStrLn stderr (T.pack err)
      TIO.hPutStrLn stderr "usage: lam [--allow-net[=hosts]] \
        \[--allow-read[=folders]] [--allow-write[=folders]] \
        \[--allow-env[=names]] [--deny-all] [--trace] [--dry-run] \
        \[file args...]"
      -- 「ワンライナー：lam [-n|-p] [-a] [-F 区切り] [-i[.bak]]
      --   [--begin 式 --end 式] -e 'プログラム' [ファイル…]」
      TIO.hPutStrLn stderr "       lam [-n|-p] [-a] [-F regex] [-i[.bak]] \
        \[--begin expr] [--end expr] [--json|--jsonl] [--path=keys] \
        \[--print-script] -e 'program' [files...]"
      exitWith (ExitFailure 2)
    Right (opts, command) -> withOutside $ \outside -> do
      -- The real world, with the guards of the flags around it.
      -- 本物の世界に、フラグの見張りをかぶせる
      let handle = guarded opts (realWorld outside)
      case command of
        -- The rest is for the script: its getArgs sees only them.
        -- 残りはスクリプトのもの。スクリプトの getArgs には、これだけが見える
        RunFile file scriptArgs -> withArgs scriptArgs (runFile handle file)
        -- ANCHOR: mainOneLiner
        -- A one-liner: the words after it are the files to read (-n,
        -- -p), or the arguments of the program (-e alone).
        -- ワンライナー：後ろの語は、読むファイル（-n・-p）か、
        -- プログラムの引数（-e だけのとき）
        RunOneLiner one rest
          -- --print-script: show the script, and run nothing.
          -- --print-script：スクリプトを見せるだけで、動かさない
          | oneScript one ->
              runScriptFlag handle preludeSession one >>= finish
          | otherwise -> withArgs rest $ do
              files <- realFiles
              runOneLiner handle files preludeSession one rest >>= finish
        -- ANCHOR_END: mainOneLiner
        Repl -> do
          -- 「Lam の REPL です。:help で使い方、:quit で終わります」
          putStrLn "Lam REPL. Type :help for help, :quit to quit."
          runInputT defaultSettings (loop handle preludeSession)
-- ANCHOR_END: head


-- ANCHOR: loop
-- 🔁 Read a line, run one step, show, and go around again.
-- 1行読み、1回分動かし、見せて、また回る
loop :: Handler IO -> Session -> InputT IO ()
loop handle session = do
  line <- getInputLine "lam> "
  case fmap (T.strip . T.pack) line of
    Nothing       -> pure ()
    Just ":quit"  -> pure ()
    Just ""       -> loop handle session
    Just text     -> case parseInput text of
      Right (Load file) -> do
        loaded <- liftIO (loadFile handle session file)
        loop handle loaded
      -- ANCHOR: replImport
      -- import, from the folder the REPL runs in (and LAM_PATH).
      -- import は、REPL を動かしたフォルダー（と LAM_PATH）から
      Right (ImportLine imp) -> do
        files <- liftIO realFiles
        imported <- liftIO (importAt files session imp)
        case imported of
          Left err -> liftIO (TIO.putStrLn err) >> loop handle session
          Right session' -> loop handle session'
      -- ANCHOR_END: replImport
      _ -> do
        -- An IO expression is done here, in the real world.
        -- IO の式は、ここで本物の世界でやる
        shown <- liftIO $ try $ do
          (session', out) <- step handle session text
          evaluate (T.length out `seq` (session', out))
        case shown of
          Right (session', o) -> do
            liftIO (unless (T.null o) (TIO.putStrLn o))
            loop handle session'
          Left NonTermination -> do
            liftIO (TIO.putStrLn
              "error: this never finishes (it needs its own value)")
            loop handle session

-- 📂 :load a file into the session.
-- ファイルを、セッションに読みこむ
loadFile :: Handler IO -> Session -> FilePath -> IO Session
loadFile handle session file = do
  source <- Utf8.readFile file
  files <- realFiles
  loaded <- loadProgram files session file source
  case loaded of
    Left err -> TIO.putStrLn err >> pure session
    Right (session', final) -> do
      -- 「読みこみました」
      TIO.putStrLn ("loaded " <> T.pack file)
      forM_ final $ \e -> do
        out <- Program.evaluate handle session' e
        TIO.putStrLn (endingText out)
      pure session'
-- ANCHOR_END: loop

-- ANCHOR: runFile
-- 📜 lam program.lam: main (if there is one) runs in the real world.
--    How it ended becomes the exit code: 0, 1 for an error, n for exit n.
-- main（あれば）は、本物の世界で動く。終わり方が終了コードになる：
-- 0、エラーなら 1、exit n なら n
runFile :: Handler IO -> FilePath -> IO ()
runFile handle file = do
  source <- Utf8.readFile file
  files  <- realFiles
  runProgram handle files preludeSession file source >>= finish

-- 🏁 How it ended becomes the exit code.
-- 終わり方を、終了コードにする
finish :: Ending -> IO ()
finish ending = case ending of
  Finished out -> unless (T.null out) (TIO.putStrLn out)
  Errored err  -> do
    TIO.hPutStrLn stderr err
    exitWith (ExitFailure 1)
  ExitedWith 0 -> exitWith ExitSuccess
  ExitedWith n -> exitWith (ExitFailure n)
-- ANCHOR_END: runFile
-- ANCHOR_END: all
