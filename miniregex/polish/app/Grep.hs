-- ANCHOR: all
-- | minigrep PATTERN [FILE...]: print the lines that match.
--   Exit status: 0 if a line was chosen, 1 if none, 2 on trouble
--   (a wrong pattern, a file that cannot be read, a line the engine
--   gave up on), the same as grep.
-- minigrep PATTERN [FILE...]：合う行を書く。終了ステータスは、行を選んだら
-- 0、1行もなければ 1、困ったことがあれば 2（まちがったパターン、読めない
-- ファイル、エンジンがあきらめた行）。grep と同じ
module Main (main) where

-- ANCHOR: imports
import Control.Exception (IOException, try)
import Control.Monad (forM, unless)
import Data.ByteString qualified as B
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.IO qualified as TIO
import Options.Applicative (execParser)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdout)
import System.IO.Error (ioeGetErrorString)

import MiniRegex
import MiniRegex.Grep
-- ANCHOR_END: imports

-- ANCHOR: main
main :: IO ()
main = do
  settings <- execParser settingsInfo
  let flags = [Caseless | caseless settings]
  case compileWith flags (pattern settings) of
    Left err -> do
      TIO.hPutStr stderr (explain (pattern settings) err)
      exitWith (ExitFailure 2)
    Right re -> do
      colored <- case color settings of
        ColorAlways -> pure True
        ColorNever  -> pure False
        ColorAuto   -> hIsTerminalDevice stdout
      let inputs = case files settings of
            [] -> ["-"]
            fs -> fs
          showNames = length inputs > 1
      results <- forM inputs $ \file -> do
        let name = if showNames then Just (nameOf file) else Nothing
        grepFile (viewOf settings colored name) re file
      let anyChosen = or [c | Right c <- results]
          troubled  = any isLeft results
      exitWith (statusOf troubled anyChosen)

-- 🚦 The exit status of grep: 2 on trouble, 0 if a line was chosen,
--    1 if none.
-- grep の終了ステータス：困ったことがあれば 2、行を選んだら 0、
-- なければ 1
statusOf :: Bool -> Bool -> ExitCode
statusOf troubled anyChosen
  | troubled  = ExitFailure 2
  | anyChosen = ExitSuccess
  | otherwise = ExitFailure 1

-- 📛 How a file is called in the output.
-- 出力の中での、ファイルの呼び名
nameOf :: FilePath -> Text
nameOf "-"  = "(standard input)"
nameOf file = T.pack file

-- 📂 One file: read it (as UTF-8; a broken byte becomes U+FFFD), grep
--    it, print. Right: was a line chosen? Left: trouble, told on
--    stderr.
-- 1つのファイル：読んで（UTF-8 で。壊れたバイトは U+FFFD に）、探して、
-- 書く。Right は行を選んだか、Left は困ったこと（stderr に書いた）
grepFile :: View -> Regex -> FilePath -> IO (Either () Bool)
grepFile view re file = do
  read' <- try (if file == "-" then B.getContents else B.readFile file)
  case read' of
    Left (e :: IOException) -> do
      hPutStrLn stderr
        ("minigrep: " <> file <> ": " <> ioeGetErrorString e)
      pure (Left ())
    Right bytes -> do
      let report = grepText view re (decodeUtf8Lenient bytes)
      mapM_ TIO.putStrLn (printedLines report)
      unless (null (gaveUpLines report)) $
        hPutStrLn stderr ("minigrep: " <> file <> ": gave up on line "
                          <> show (gaveUpLines report))
      pure $ if null (gaveUpLines report)
        then Right (chosenCount report > 0)
        else Left ()
-- ANCHOR_END: main
-- ANCHOR_END: all
