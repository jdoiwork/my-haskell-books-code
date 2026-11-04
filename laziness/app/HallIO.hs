-- ANCHOR: all
-- 📜 Add a record to the hall of fame: read the file, and write it back.
-- 殿堂に記録を足す：ファイルを読んで、書き戻す
module Main (main) where

import Data.Text          qualified as T
import Data.Text.IO       qualified as TIO
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["string", path] -> withString path
    ["text",   path] -> withText path
    _                -> putStrLn "usage: hallio (string|text) FILE"

-- ANCHOR: lazy
-- 🐌 String's readFile reads lazily: the file is still open here.
-- String 版の readFile は遅延して読む。ここでは、まだファイルが開いている
withString :: FilePath -> IO ()
withString path = do
  contents <- readFile path
  writeFile path (contents ++ "Rust,won\n")
-- ANCHOR_END: lazy

-- ANCHOR: strict
-- ⚡ Text's readFile reads it all, and closes the file.
-- Text 版の readFile は全部読んで、ファイルを閉じる
withText :: FilePath -> IO ()
withText path = do
  contents <- TIO.readFile path
  TIO.writeFile path (contents <> T.pack "Rust,won\n")
-- ANCHOR_END: strict
-- ANCHOR_END: all
