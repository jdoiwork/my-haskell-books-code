-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | lam: the REPL, run a file, or run a one-liner.
--     lam  /  lam program.lam args...  /  lam -ne '...' files...
-- lam：REPL、ファイルを動かす、またはワンライナーを動かす。ファイルの
-- 後ろは、スクリプトの引数
module Main (main) where

import Control.Exception (NonTermination (..), evaluate, try)
import Control.Monad (forM_, unless, when)
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
import Lam.Program (Ending (..), Session (..))
import qualified Lam.Program as Program
import Lam.Repl (Input (..), endingText, parseInput, step)
import Lam.Guard (Options (..), guarded, usage)
import Lam.OneLiner (Command (..), oneLinerUsage, readCommand, runOneLiner)
import Lam.Io (Handler)
import Lam.World (newDesk, newDice, realWorld, withOutside)

main :: IO ()
main = do
  args <- getArgs
  -- ANCHOR: flags
  -- The flags of lam come before the file (or before the input files
  -- of a one-liner); the rest is for the script. An unknown flag stops
  -- lam, with how to use it (exit code 2).
  -- lam のフラグはファイル（ワンライナーなら入力のファイル）の前に。
  -- 残りはスクリプトのもの。知らないフラグなら、使い方を見せて止まる
  -- （終了コード 2）
  fromEnv <- lookupEnv "LAM_SEED"
  (opts, command) <- case readCommand args fromEnv of
    Right found -> pure found
    Left why    -> do
      TIO.hPutStrLn stderr why
      TIO.hPutStr stderr (usage <> oneLinerUsage)
      exitWith (ExitFailure 2)
  dice <- newDice (randomSeed opts)
  -- The desk keeps the files a script has open.
  -- 机には、スクリプトが開いているファイルを置いておく
  desk <- newDesk
  -- The guards for the flags, put around the real world.
  -- フラグに合わせた見張りを、本物の世界にかぶせる
  guard <- guarded opts
  withOutside $ \outside ->
    start (guard (realWorld dice desk outside)) command
  -- ANCHOR_END: flags

-- ANCHOR: start
-- ▶️ Run the file, the one-liner, or the REPL with no file.
-- ファイルかワンライナーを動かす。ファイルがなければ REPL
start :: Handler IO -> Command -> IO ()
start world command = case command of
  -- Its getArgs sees only the ones after the file.
  -- スクリプトの getArgs には、ファイルより後ろだけが見える
  RunFile file more -> withArgs more (runFile world file)
  -- ANCHOR: startOneLiner
  -- The words after a one-liner: the files to read (-n, -p), or the
  -- arguments of the program (-e alone).
  -- ワンライナーの後ろの語：読むファイル（-n・-p）か、プログラムの
  -- 引数（-e だけのとき）
  RunOneLiner one more -> withArgs more $ do
    files <- realFiles
    runOneLiner world files preludeSession one >>= finish
  -- ANCHOR_END: startOneLiner
  -- ANCHOR_END: start
  Repl -> do
    -- 「Lam の REPL です。:help で使い方、:quit で終わります」
    putStrLn "Lam REPL. Type :help for help, :quit to quit."
    runInputT defaultSettings (loop world preludeSession)
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
    Just text     -> answerLine world session text

-- 💬 Answer one line (not :quit): load a file, import, or run it.
-- 1行（:quit ではない）に答える：ファイルを読む、import、動かす
answerLine :: Handler IO -> Session -> T.Text -> InputT IO ()
answerLine world session text =
  case parseInput (sessionOperators session) text of
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
  -- A file with nothing in it (left by a | tee whose lam stopped, ...)
  -- runs nothing: say so, so the quiet is not taken for success.
  -- 中身のないファイル（lam が止まった | tee が残したものなど）は、何も
  -- しない。黙って終わると、うまくいったと思われるので、知らせる
  when (T.null (T.strip source)) $
    TIO.hPutStrLn stderr ("lam: warning: " <> T.pack file
                            <> " is empty: there is nothing to run")
  files  <- realFiles
  loaded <- loadProgram files preludeSession preludeSession file source
  ending <- case loaded of
    Left err             -> pure (Errored err)
    Right (done, final)  -> runProgram world done final
  finish ending

-- 🏁 How it ended becomes the exit code.
-- 終わり方を、終了コードにする
finish :: Ending -> IO ()
finish ending = case ending of
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
