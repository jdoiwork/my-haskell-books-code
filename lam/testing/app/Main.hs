-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | lam: the REPL, run a file, run a one-liner, or make one program
--   file of a script.
--     lam  /  lam program.lam args...  /  lam -ne '...' files...
--     lam compile [flags] app.lam -o app
--     lam test [--seed n | --seeds n] [files or folders...]
-- lam：REPL、ファイルを動かす、ワンライナーを動かす、スクリプトを
-- 1つの実行ファイルにする。テストを動かす。ファイルの後ろは、スクリプトの引数
module Main (main) where

import Control.Exception (NonTermination (..), evaluate, try)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf, nub, stripPrefix)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.IO.Utf8 as Utf8
import System.Console.Haskeline
import Data.Version (showVersion)
import qualified Paths_lam as Paths
import System.Environment (getArgs, lookupEnv, withArgs)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (dropExtension, splitSearchPath, takeFileName)
import Data.Text (Text)
import System.IO (stderr)

import Lam.Module (Files (..), diskFiles, importAt, loadProgram, runProgram)
import Lam.Bundle
  ( Bundle (..), bundleFiles, includedFiles, readOwnBundle, recording
  , withIncluded, writeProgram )
import Lam.Prelude (preludeSession)
import Lam.Program (Ending (..), Session (..))
import qualified Lam.Program as Program
import Lam.Repl (Input (..), completions, endingText, parseInput, step)
import Lam.TestRun (readTestArgs, runTests, testUsage)
import Lam.Guard (Options (..), guarded, readOptions, usage)
import Lam.Settings (loadSettings)
import Lam.OneLiner (Command (..), oneLinerUsage, readCommand, runOneLiner)
import Lam.Io (Handler)
import Lam.World (newDesk, newDice, realWorld, withOutside)

-- ANCHOR: main
-- 🚪 A program file made by lam compile has a bundle at its end: then
--    the flags, the file, the allowed commands of lam.json, and the
--    included files come from the bundle, and every argument is the
--    script's. Otherwise, lam compile, or lam as before.
-- lam compile で作った実行ファイルには、最後に包みがある。そのときは、
-- フラグ・ファイル・lam.json の許すコマンド・入れたファイルは包みから
-- 来て、引数は全部スクリプトのもの。そうでなければ、lam compile か、
-- これまでの lam
main :: IO ()
main = do
  given <- getArgs
  bundled <- readOwnBundle
  case (bundled, given) of
    (Just (b, included), _) ->
      runWith (bundleFiles b) (Just (bundleAllowed b)) (withIncluded included)
        (bundleFlags b <> [bundleMain b] <> given)
    -- lam --help and lam --version: alone, as the first word.
    -- lam --help と lam --version：最初の語に、1つだけで
    (Nothing, ["--help"]) ->
      TIO.putStr (usage <> oneLinerUsage <> testUsage)
    (Nothing, ["--version"]) ->
      putStrLn ("lam " <> showVersion Paths.version)
    (Nothing, "compile" : rest) -> compile rest
    (Nothing, "test" : rest) -> testCommand rest
    (Nothing, _) -> realFiles >>= \files -> runWith files Nothing id given
-- ANCHOR_END: main

-- ANCHOR: mainTest
-- 🧪 lam test [--seed n | --seeds n] [files or folders...]. The tests
--    run in fake worlds, so the flags of the real world (--allow-...,
--    --dry-run) and lam.json mean nothing here: only the seed is read
--    (LAM_SEED, too). The exit code is 1 if a test failed.
-- テストは偽物の世界で動くので、本物の世界のフラグ（--allow-…、
-- --dry-run）と lam.json は、ここでは意味がない。読むのは種だけ
-- （LAM_SEED も）。失敗したテストがあれば、終了コードは 1
testCommand :: [String] -> IO ()
testCommand rest = do
  fromEnv <- lookupEnv "LAM_SEED"
  case readTestArgs rest fromEnv of
    Left err -> do
      TIO.hPutStrLn stderr err
      TIO.hPutStr stderr testUsage
      exitWith (ExitFailure 2)
    Right testArgs -> do
      files <- realFiles
      code <- runTests files preludeSession testArgs
      exitWith (if code == 0 then ExitSuccess else ExitFailure code)
-- ANCHOR_END: mainTest

-- ▶️ Read the flags and the settings, and start: the files are the
--    disk (or a bundle). A bundle gives its allowed commands, and puts
--    its included files in front of the disk.
-- フラグと設定を読んで、始める。ファイルはディスク（か、包み）。包みは
-- 許すコマンドを渡し、入れたファイルをディスクの前に置く
runWith
  :: Files IO -> Maybe [[Text]] -> (Handler IO -> Handler IO) -> [String]
  -> IO ()
runWith files baked inside args = do
  -- ANCHOR: flags
  -- The flags of lam come before the file (or before the input files
  -- of a one-liner); the rest is for the script. An unknown flag stops
  -- lam, with how to use it (exit code 2).
  -- lam のフラグはファイル（ワンライナーなら入力のファイル）の前に。
  -- 残りはスクリプトのもの。知らないフラグなら、使い方を見せて止まる
  -- （終了コード 2）
  fromEnv <- lookupEnv "LAM_SEED"
  (flagged, command) <- case readCommand args fromEnv of
    Right found -> pure found
    Left why    -> do
      TIO.hPutStrLn stderr why
      TIO.hPutStr stderr (usage <> oneLinerUsage)
      exitWith (ExitFailure 2)
  -- ANCHOR: settings
  -- The commands lam.json allows on a dry run: baked into a bundle, or
  -- read now. A bad lam.json stops lam, as a bad flag does.
  -- lam.json が試しに動かすときに許すコマンド：包みに焼きこんだものか、
  -- いま読むもの。まちがった lam.json は、まちがったフラグと同じく
  -- lam を止める
  allowed <- maybe (settingsOf flagged (scriptOf command)) pure baked
  let opts = flagged { dryAllowed = allowed }
  -- ANCHOR_END: settings
  dice <- newDice (randomSeed opts)
  -- The desk keeps the files a script has open.
  -- 机には、スクリプトが開いているファイルを置いておく
  desk <- newDesk
  -- The guards for the flags, put around the real world.
  -- フラグに合わせた見張りを、本物の世界にかぶせる
  guard <- guarded opts
  withOutside $ \outside ->
    start files (guard (inside (realWorld dice desk outside))) command
  -- ANCHOR_END: flags

-- ANCHOR: settingsOf
-- 📖 Read the settings for this script: --settings=FILE, or the
--    lam.json next to it (none there: nothing is allowed).
-- このスクリプトの設定を読む：--settings=FILE か、隣の lam.json
-- （なければ、何も許さない）
settingsOf :: Options -> Maybe FilePath -> IO [[Text]]
settingsOf opts script =
  loadSettings (settingsFile opts) script >>= either stop pure
  where
    stop why = TIO.hPutStrLn stderr why >> exitWith (ExitFailure 2)

-- 📜 The script of a command (the REPL and a one-liner have none).
-- コマンドのスクリプト（REPL とワンライナーには、ない）
scriptOf :: Command -> Maybe FilePath
scriptOf command = case command of
  RunFile file _ -> Just file
  _              -> Nothing
-- ANCHOR_END: settingsOf

-- ANCHOR: start
-- ▶️ Run the file, the one-liner, or the REPL with no file.
-- ファイルかワンライナーを動かす。ファイルがなければ REPL
start :: Files IO -> Handler IO -> Command -> IO ()
start files world command = case command of
  -- Its getArgs sees only the ones after the file.
  -- スクリプトの getArgs には、ファイルより後ろだけが見える
  RunFile file more -> withArgs more (runFile files world file)
  -- ANCHOR: startOneLiner
  -- The words after a one-liner: the files to read (-n, -p), or the
  -- arguments of the program (-e alone).
  -- ワンライナーの後ろの語：読むファイル（-n・-p）か、プログラムの
  -- 引数（-e だけのとき）
  RunOneLiner one more -> withArgs more $
    runOneLiner world files preludeSession one >>= finish
  -- ANCHOR_END: startOneLiner
  -- ANCHOR_END: start
  Repl -> do
    -- 「Lam の REPL です。:help で使い方、:quit で終わります」
    putStrLn "Lam REPL. Type :help for help, :quit to quit."
    -- Tab reads the names of the session the loop keeps here.
    -- Tab は、ループがここに置くセッションの名前を読む
    current <- newIORef preludeSession
    runInputT (setComplete (completer current) defaultSettings)
      (loop (Looping current world) (Kept preludeSession Nothing))
-- ANCHOR_END: head

-- ANCHOR: loop
-- 🧭 What the loop needs: where Tab reads the names, and the world.
-- ループが要るもの：Tab が名前を読む場所と、世界
data Looping = Looping
  { loopNames :: IORef Session
  , loopWorld :: Handler IO }

-- 🧠 What the loop keeps: the session, and the last file loaded with
--    the session from before it (for :reload).
-- ループが持つもの：セッションと、最後に :load したファイルと、その前の
-- セッション（:reload のため）
data Kept = Kept
  { keptSession :: Session
  , keptLoad    :: Maybe (Session, FilePath) }

-- 🔁 Read a line, run one step, show, and go around again.
-- 1行読み、1回分動かし、見せて、また回る
loop :: Looping -> Kept -> InputT IO ()
loop looping kept = do
  liftIO (writeIORef (loopNames looping) (keptSession kept))
  line <- getInputLine "lam> "
  case fmap (T.strip . T.pack) line of
    Nothing       -> pure ()
    Just ":quit"  -> pure ()
    Just ""       -> loop looping kept
    Just text     -> answerLine looping kept text

-- 💬 Answer one line (not :quit): load, reload, test, import, or run it.
-- 1行（:quit ではない）に答える：読む、読み直す、テスト、import、動かす
answerLine :: Looping -> Kept -> T.Text -> InputT IO ()
answerLine looping kept text =
  case parseInput (sessionOperators session) text of
    Right (Load file) -> do
      loaded <- liftIO (loadFile world session file)
      loop looping (Kept loaded (Just (session, file)))
    -- ANCHOR: replReload
    -- :reload: the same file, on the session from before it, so that
    -- a name taken out of the file goes away, too.
    -- 同じファイルを、その前のセッションの上に。ファイルから消した名前も
    -- 消える
    Right Reload -> case lastLoad of
      Nothing -> do
        -- 「読み直すものがありません（先に :load を）」
        liftIO (TIO.putStrLn "nothing to reload (use :load first)")
        again session
      Just (before, file) -> do
        loaded <- liftIO (loadFile world before file)
        loop looping (Kept loaded lastLoad)
    -- ANCHOR_END: replReload
    -- ANCHOR: replTest
    -- :test: the same as lam test, with a fresh prelude (the
    -- definitions of the REPL do not go into the tests).
    -- lam test と同じ。プレリュードは新しいもの（REPL の定義は、テストに
    -- 入らない）
    Right (RunTests ws) -> do
      fromEnv <- liftIO (lookupEnv "LAM_SEED")
      case readTestArgs ws fromEnv of
        Left err -> liftIO (TIO.putStrLn err)
        Right testArgs -> liftIO $ do
          files <- realFiles
          _ <- runTests files preludeSession testArgs
          pure ()
      again session
    -- ANCHOR_END: replTest
    -- ANCHOR: replImport
    -- import, from the folder the REPL runs in (and LAM_PATH).
    -- import は、REPL を動かしたフォルダー（と LAM_PATH）から
    Right (ImportLine imp) -> do
      files <- liftIO realFiles
      imported <- liftIO (importAt files preludeSession session imp)
      case imported of
        Left err -> liftIO (TIO.putStrLn err) >> again session
        Right session' -> again session'
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
          again session'
        Left NonTermination -> do
          liftIO (TIO.putStrLn
            "error: this never finishes (it needs its own value)")
          again session
  where
    world    = loopWorld looping
    session  = keptSession kept
    lastLoad = keptLoad kept
    again s  = loop looping (Kept s lastLoad)

-- 📂 :load a file into the session.
-- ファイルを、セッションに読みこむ
loadFile :: Handler IO -> Session -> FilePath -> IO Session
loadFile world session file = do
  source <- Utf8.readFile file
  -- A file with nothing in it (left by a | tee whose lam stopped, ...)
  -- runs nothing: say so, so the quiet is not taken for success.
  -- 中身のないファイル（lam が止まった | tee が残したものなど）は、何も
  -- しない。黙って終わると、うまくいったと思われるので、知らせる
  when (T.null (T.strip source)) $
    TIO.hPutStrLn stderr ("lam: warning: " <> T.pack file
                            <> " is empty: there is nothing to run")
  files  <- realFiles
  loaded <- loadProgram files preludeSession session file source
  case loaded of
    Left err -> TIO.putStrLn err >> pure session
    Right (loaded', final) -> do
      -- The warnings of the file, once (see keyWarnings).
      -- ファイルの警告を、1度だけ（keyWarnings を見る）
      mapM_ (TIO.hPutStrLn stderr) (nub (sessionWarnings loaded'))
      let session' = loaded' { sessionWarnings = [] }
      -- 「読みこみました」
      TIO.putStrLn ("loaded " <> T.pack file)
      -- The expression at the end is done in the real world too.
      -- 最後の式も、本物の世界でやる
      forM_ final $ \e -> do
        out <- Program.evaluate world session' e
        unless (out == Finished "") (TIO.putStrLn (endingText out))
      pure session'
-- ANCHOR_END: loop

-- ANCHOR: completer
-- ⌨️ Tab: after :load or :test, a file name; otherwise a name or a
--    command of the session. A word ends at a space or a bracket (not
--    at a dot: Map.insert is one word).
-- Tab：:load と :test の後ろはファイルの名前、ほかはセッションの名前か
-- コマンド。単語は空白やかっこで切れる（点では切れない）
completer :: IORef Session -> CompletionFunc IO
completer current = completeWordWithPrev Nothing " \t()[]{},|" $
  \before word -> do
    let typed = reverse before
    if any (`isPrefixOf` typed) [":load ", ":test ", ":l "]
      then listFiles word
      else do
        session <- readIORef current
        pure [ simpleCompletion (T.unpack n)
             | n <- completions session, T.pack word `T.isPrefixOf` n ]
-- ANCHOR_END: completer

-- ANCHOR: runFile
-- 📜 lam program.lam: load it, then run it in the real world.
--    How it ended becomes the exit code: 0, 1 for an error, n for exit n.
-- 読みこんでから、本物の世界で動かす。終わり方が終了コードになる：
-- 0、エラーなら 1、exit n なら n
runFile :: Files IO -> Handler IO -> FilePath -> IO ()
runFile files world file = do
  source <- maybe (Utf8.readFile file) pure =<< readSource files file
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

-- ANCHOR: compile
-- 📦 lam compile [flags] app.lam [-o app] [--include public]: load the
--    program (so its mistakes are told now, not on the machine of a
--    friend), writing down each file it reads, and write lam with them
--    at its end. The flags (--allow-net=..., --allow-read=...) are
--    checked, and baked in; so are the commands lam.json allows on a
--    dry run, and the files of --include (a file, or all the files
--    under a folder).
-- プログラムを読みこみ（まちがいは、友だちのマシンではなく、いま
-- わかる）、読んだファイルを1つずつ書き留めて、それを最後に付けた lam を
-- 書く。フラグは確かめてから焼きこむ。lam.json が試しに動かすときに
-- 許すコマンドと、--include のファイル（ファイル1つか、フォルダーの下の
-- 全部）も焼きこむ
compile :: [String] -> IO ()
compile args = case splitOut args of
  Just (out, includes, flags, file)
    | Right (opts, _) <- readOptions flags Nothing -> do
        seen  <- newIORef []
        path  <- maybe [] splitSearchPath <$> lookupEnv "LAM_PATH"
        let files = recording seen (diskFiles path)
        source <- Utf8.readFile file
        loaded <- loadProgram files preludeSession preludeSession file source
        case loaded of
          Left err -> TIO.hPutStrLn stderr err >> exitWith (ExitFailure 1)
          Right _  -> do
            modules <- reverse <$> readIORef seen
            allowed <- settingsOf opts (Just file)
            included <- concat <$> mapM including includes
            let sources = nub ((file, source) : modules)
                name = maybe (dropExtension (takeFileName file)) id out
                bundle = Bundle file flags sources path allowed []
            size <- writeProgram name bundle included
            -- 「app を作りました（93 MB）：app.lam, Todos.lam」
            TIO.putStrLn (T.pack ("made " <> name <> " ("
              <> show (size `div` 1000000) <> " MB): ")
              <> T.intercalate ", " (map (T.pack . fst) sources))
            -- 「入れたファイル：public/index.html, public/logo.png」
            unless (null included) $ TIO.putStrLn ("  included: "
              <> T.intercalate ", " (map (T.pack . fst) included))
  Just (_, _, flags, _) | Left why <- readOptions flags Nothing ->
    TIO.hPutStrLn stderr why >> exitWith (ExitFailure 2)
  _ -> do
    TIO.hPutStrLn stderr ("usage: lam compile [flags] FILE.lam [-o NAME] "
                          <> "[--include PATH]")
    exitWith (ExitFailure 2)
  where
    -- A path of --include that is not there stops lam compile.
    -- --include の道がなければ、lam compile は止まる
    including p = includedFiles p >>= either
      (\why -> TIO.hPutStrLn stderr ("lam compile: --include " <> why)
               >> exitWith (ExitFailure 1))
      pure

-- ✂️ -o NAME, --include PATH (again and again), the flags, and the
--    file, in any order.
-- -o NAME、--include PATH（何度でも）、フラグ、ファイル（順番は自由）
splitOut
  :: [String] -> Maybe (Maybe FilePath, [FilePath], [String], FilePath)
splitOut = go Nothing [] [] Nothing
  where
    go out inc flags file ws = case ws of
      "-o" : name : rest -> go (Just name) inc flags file rest
      "--include" : p : rest -> go out (p : inc) flags file rest
      w : rest | Just p <- stripInclude w -> go out (p : inc) flags file rest
      w : rest | take 1 w == "-" -> go out inc (w : flags) file rest
      w : rest | Nothing <- file -> go out inc flags (Just w) rest
      [] | Just f <- file -> Just (out, reverse inc, reverse flags, f)
      _ -> Nothing
    stripInclude w = case stripPrefix "--include=" w of
      Just p | not (null p) -> Just p
      _                     -> Nothing
-- ANCHOR_END: compile

-- 💽 The disk, and the folders of LAM_PATH (lib:more, as PATH).
-- ディスクと、LAM_PATH のフォルダー（PATH と同じ形の lib:more）
realFiles :: IO (Files IO)
realFiles = do
  path <- lookupEnv "LAM_PATH"
  pure (diskFiles (maybe [] splitSearchPath path))
-- ANCHOR_END: runFile
-- ANCHOR_END: all
