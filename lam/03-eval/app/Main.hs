{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | lam: the REPL, or run a file.  lam  /  lam program.lam
-- lam：REPL、またはファイルを動かす。
module Main (main) where

import Control.Exception (NonTermination (..), evaluate, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.IO.Utf8 as Utf8
import System.Console.Haskeline
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)

import Lam.Eval (Env)
import Lam.Repl (runProgram, step)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> runFile file
    _      -> do
      -- 「Lam の REPL です。:quit で終わります」
      putStrLn "Lam REPL. Type :quit to quit."
      runInputT defaultSettings (loop Map.empty)

-- ANCHOR: loop
-- 🔁 Read a line, run one step, show, and go around again.
-- 1行読み、1回分動かし、見せて、また回る
loop :: Env -> InputT IO ()
loop env = do
  line <- getInputLine "lam> "
  case fmap T.strip (fmap T.pack line) of
    Nothing       -> pure ()               -- Ctrl-D
    Just ":quit"  -> pure ()
    Just ""       -> loop env
    Just text     -> do
      let (env', out) = step env text
      -- let x = x + 1 never finishes. GHC notices and throws
      -- NonTermination; catch it, and keep the old environment.
      -- let x = x + 1 は終わらない。GHC が気づいて NonTermination を
      -- 投げるので、受け止めて、前の環境のまま続ける
      shown <- liftIO (try (evaluate (T.length out `seq` out)))
      case shown of
        Right o -> do
          liftIO (TIO.putStrLn o)
          loop env'
        Left NonTermination -> do
          -- 「この定義は終わりません（自分の値を使っています）」
          liftIO (TIO.putStrLn
            "error: this never finishes (it needs its own value)")
          loop env
-- ANCHOR_END: loop

-- 📜 lam program.lam
runFile :: FilePath -> IO ()
runFile file = do
  source <- Utf8.readFile file
  case runProgram file source of
    Right out -> TIO.putStrLn out
    Left err  -> do
      TIO.hPutStrLn stderr err
      exitFailure
-- ANCHOR_END: all
