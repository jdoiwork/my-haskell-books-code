-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | lam: the REPL, or run a file.  lam  /  lam program.lam args...
-- lam：REPL、またはファイルを動かす。ファイルの後ろは、スクリプトの引数
module Main (main) where

import Control.Exception (NonTermination (..), evaluate, try)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.IO.Utf8 as Utf8
import System.Console.Haskeline
import System.Environment (getArgs, lookupEnv, withArgs)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (splitSearchPath)
import System.IO (stderr)

import Lam.Module (Files, diskFiles, importAt, loadProgram, runProgram)
import Lam.Prelude (preludeSession)
import Lam.Program (Ending (..), Session)
import qualified Lam.Program as Program
import Lam.Repl (Input (..), endingText, parseInput, step)
import Lam.World (realWorld)

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- The rest is for the script: its getArgs sees only them.
    -- 残りはスクリプトのもの。スクリプトの getArgs には、これだけが見える
    file : rest -> withArgs rest (runFile file)
    []          -> do
      -- 「Lam の REPL です。:help で使い方、:quit で終わります」
      putStrLn "Lam REPL. Type :help for help, :quit to quit."
      runInputT defaultSettings (loop preludeSession)
-- ANCHOR_END: head

-- ANCHOR: loop
-- 🔁 Read a line, run one step, show, and go around again.
-- 1行読み、1回分動かし、見せて、また回る
loop :: Session -> InputT IO ()
loop session = do
  line <- getInputLine "lam> "
  case fmap (T.strip . T.pack) line of
    Nothing       -> pure ()
    Just ":quit"  -> pure ()
    Just ""       -> loop session
    Just text     -> case parseInput text of
      Right (Load file) -> do
        loaded <- liftIO (loadFile session file)
        loop loaded
      -- ANCHOR: replImport
      -- import, from the folder the REPL runs in (and LAM_PATH).
      -- import は、REPL を動かしたフォルダー（と LAM_PATH）から
      Right (ImportLine imp) -> do
        files <- liftIO realFiles
        imported <- liftIO (importAt files preludeSession session imp)
        case imported of
          Left err -> liftIO (TIO.putStrLn err) >> loop session
          Right session' -> loop session'
      -- ANCHOR_END: replImport
      _ -> do
        -- An IO expression is done here, in the real world.
        -- IO の式は、ここで本物の世界でやる
        shown <- liftIO $ try $ do
          (session', out) <- step realWorld session text
          evaluate (T.length out `seq` (session', out))
        case shown of
          Right (session', o) -> do
            liftIO (unless (T.null o) (TIO.putStrLn o))
            loop session'
          Left NonTermination -> do
            liftIO (TIO.putStrLn
              "error: this never finishes (it needs its own value)")
            loop session

-- 📂 :load a file into the session.
-- ファイルを、セッションに読みこむ
loadFile :: Session -> FilePath -> IO Session
loadFile session file = do
  source <- Utf8.readFile file
  files  <- realFiles
  loaded <- loadProgram files preludeSession session file source
  case loaded of
    Left err -> TIO.putStrLn err >> pure session
    Right (session', final) -> do
      -- 「読みこみました」
      TIO.putStrLn ("loaded " <> T.pack file)
      -- The expression at the end is done in the real world too.
      -- 最後の式も、本物の世界でやる
      forM_ final $ \e -> do
        out <- Program.evaluate realWorld session' e
        unless (out == Finished "") (TIO.putStrLn (endingText out))
      pure session'
-- ANCHOR_END: loop

-- ANCHOR: runFile
-- 📜 lam program.lam: load it, then run it in the real world.
--    How it ended becomes the exit code: 0, 1 for an error, n for exit n.
-- 読みこんでから、本物の世界で動かす。終わり方が終了コードになる：
-- 0、エラーなら 1、exit n なら n
runFile :: FilePath -> IO ()
runFile file = do
  source <- Utf8.readFile file
  files  <- realFiles
  loaded <- loadProgram files preludeSession preludeSession file source
  ending <- case loaded of
    Left err             -> pure (Errored err)
    Right (done, final)  -> runProgram realWorld done final
  case ending of
    Finished _   -> pure ()
    Errored err  -> do
      TIO.hPutStrLn stderr err
      exitWith (ExitFailure 1)
    ExitedWith 0 -> exitWith ExitSuccess
    ExitedWith n -> exitWith (ExitFailure n)

-- 💽 The disk, and the folders of LAM_PATH (lib:more, as PATH).
-- ディスクと、LAM_PATH のフォルダー（PATH と同じ形の lib:more）
realFiles :: IO (Files IO)
realFiles = do
  path <- lookupEnv "LAM_PATH"
  pure (diskFiles (maybe [] splitSearchPath path))
-- ANCHOR_END: runFile
-- ANCHOR_END: all
