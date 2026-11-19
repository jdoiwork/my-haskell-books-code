-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | lam: the REPL, or run a file.  lam  /  lam program.lam args...
-- lam：REPL、またはファイルを動かす。ファイルの後ろは、スクリプトの引数
module Main (main) where

import Control.Exception (NonTermination (..), evaluate, try)
import Control.Monad (forM_, unless)
import Data.List (isPrefixOf, stripPrefix)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.IO.Utf8 as Utf8
import System.Console.Haskeline
import System.Environment (getArgs, lookupEnv, withArgs)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (splitSearchPath)
import System.IO (stderr)
import Text.Read (readMaybe)

import Lam.Module (Files, diskFiles, importAt, loadProgram, runProgram)
import Lam.Prelude (preludeSession)
import Lam.Program (Ending (..), Session)
import qualified Lam.Program as Program
import Lam.Repl (Input (..), endingText, parseInput, step)
import Lam.Io (Handler)
import Lam.World (newDesk, newDice, realWorld, withOutside)

main :: IO ()
main = do
  args <- getArgs
  -- ANCHOR: seedFlag
  -- The flags of lam come before the file; the rest is for the script.
  -- lam のフラグはファイルの前に。残りはスクリプトのもの
  let (flags, rest) = span ("--" `isPrefixOf`) args
  fromEnv <- lookupEnv "LAM_SEED"
  dice <- case seedOf flags fromEnv of
    Right seed -> newDice seed
    Left why   -> do
      TIO.hPutStrLn stderr why
      exitWith (ExitFailure 2)
  -- The desk keeps the files a script has open.
  -- 机には、スクリプトが開いているファイルを置いておく
  desk <- newDesk
  -- ANCHOR_END: seedFlag
  -- ANCHOR: outside
  -- The outside (HTTP and the databases) is closed at the end, even
  -- when a script forgot to close a database.
  -- 外の世界（HTTP とデータベース）は、終わりに閉じる。スクリプトが
  -- データベースを閉じ忘れても
  withOutside $ \outside ->
    start (realWorld dice desk outside) rest
  -- ANCHOR_END: outside

-- ▶️ Run the file, or the REPL with no file.
-- ファイルを動かす。ファイルがなければ REPL
start :: Handler IO -> [String] -> IO ()
start world rest = case rest of
  -- Its getArgs sees only the ones after the file.
  -- スクリプトの getArgs には、ファイルより後ろだけが見える
  file : more -> withArgs more (runFile world file)
  []          -> do
    -- 「Lam の REPL です。:help で使い方、:quit で終わります」
    putStrLn "Lam REPL. Type :help for help, :quit to quit."
    runInputT defaultSettings (loop world preludeSession)

-- ANCHOR: seedOf
-- 🎲 The seed of the dice: --seed=618 first, then LAM_SEED=618, or
--    none (the system picks one). Any other flag is a mistake.
-- さいころの種：まず --seed=618、次に LAM_SEED=618、どちらもなければ
-- なし（システムが選ぶ）。ほかのフラグは、まちがい
seedOf :: [String] -> Maybe String -> Either T.Text (Maybe Int)
seedOf flags fromEnv = case flags of
  [] -> maybe (Right Nothing) (number "LAM_SEED") fromEnv
  [flag] | Just n <- stripPrefix "--seed=" flag -> number "--seed" n
  flag : _ -> Left ("lam: unknown flag: " <> T.pack flag)
  where
    -- 「種は整数で」
    number name text = case readMaybe text of
      Just n  -> Right (Just n)
      Nothing -> Left ("lam: " <> name <> " needs a whole number: "
                         <> T.pack text)
-- ANCHOR_END: seedOf
-- ANCHOR_END: head

-- ANCHOR: loop
-- 🔁 Read a line, run one step, show, and go around again.
-- 1行読み、1回分動かし、見せて、また回る
loop :: Handler IO -> Session -> InputT IO ()
loop world session = do
  line <- getInputLine "lam> "
  case fmap (T.strip . T.pack) line of
    Nothing       -> pure ()
    Just ":quit"  -> pure ()
    Just ""       -> loop world session
    Just text     -> case parseInput text of
      Right (Load file) -> do
        loaded <- liftIO (loadFile world session file)
        loop world loaded
      -- ANCHOR: replImport
      -- import, from the folder the REPL runs in (and LAM_PATH).
      -- import は、REPL を動かしたフォルダー（と LAM_PATH）から
      Right (ImportLine imp) -> do
        files <- liftIO realFiles
        imported <- liftIO (importAt files preludeSession session imp)
        case imported of
          Left err -> liftIO (TIO.putStrLn err) >> loop world session
          Right session' -> loop world session'
      -- ANCHOR_END: replImport
      _ -> do
        -- An IO expression is done here, in the real world.
        -- IO の式は、ここで本物の世界でやる
        shown <- liftIO $ try $ do
          (session', out) <- step world session text
          evaluate (T.length out `seq` (session', out))
        case shown of
          Right (session', o) -> do
            liftIO (unless (T.null o) (TIO.putStrLn o))
            loop world session'
          Left NonTermination -> do
            liftIO (TIO.putStrLn
              "error: this never finishes (it needs its own value)")
            loop world session

-- 📂 :load a file into the session.
-- ファイルを、セッションに読みこむ
loadFile :: Handler IO -> Session -> FilePath -> IO Session
loadFile world session file = do
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
        out <- Program.evaluate world session' e
        unless (out == Finished "") (TIO.putStrLn (endingText out))
      pure session'
-- ANCHOR_END: loop

-- ANCHOR: runFile
-- 📜 lam program.lam: load it, then run it in the real world.
--    How it ended becomes the exit code: 0, 1 for an error, n for exit n.
-- 読みこんでから、本物の世界で動かす。終わり方が終了コードになる：
-- 0、エラーなら 1、exit n なら n
runFile :: Handler IO -> FilePath -> IO ()
runFile world file = do
  source <- Utf8.readFile file
  files  <- realFiles
  loaded <- loadProgram files preludeSession preludeSession file source
  ending <- case loaded of
    Left err             -> pure (Errored err)
    Right (done, final)  -> runProgram world done final
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
