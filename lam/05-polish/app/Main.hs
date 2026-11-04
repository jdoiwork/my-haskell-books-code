{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | lam: the REPL, or run a file.  lam  /  lam program.lam
-- lam：REPL、またはファイルを動かす。
module Main (main) where

import Control.Exception (NonTermination (..), evaluate, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.IO.Utf8 as Utf8
import System.Console.Haskeline
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)

import Lam.Prelude (preludeSession)
import Lam.Program (Session, loadProgram)
import Lam.Repl (Input (..), parseInput, step)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> runFile file
    _      -> do
      -- 「Lam の REPL です。:help で使い方、:quit で終わります」
      putStrLn "Lam REPL. Type :help for help, :quit to quit."
      runInputT defaultSettings (loop preludeSession)

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
      _ -> do
        let (session', out) = step session text
        shown <- liftIO (try (evaluate (T.length out `seq` out)))
        case shown of
          Right o -> do
            liftIO (TIO.putStrLn o)
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
  case loadProgram session file source of
    Left err -> TIO.putStrLn err >> pure session
    Right (session', out) -> do
      -- 「読みこみました」
      TIO.putStrLn ("loaded " <> T.pack file)
      mapM_ TIO.putStrLn out
      pure session'
-- ANCHOR_END: loop

-- 📜 lam program.lam
runFile :: FilePath -> IO ()
runFile file = do
  source <- Utf8.readFile file
  case loadProgram preludeSession file source of
    Right (_, Just out) -> TIO.putStrLn out
    Right (_, Nothing)  -> pure ()
    Left err            -> do
      TIO.hPutStrLn stderr err
      exitFailure
-- ANCHOR_END: all
