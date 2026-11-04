-- ANCHOR: all
-- | The MCP server: read a JSON-RPC message from each line of stdin,
--   write the answer to stdout.
-- MCP サーバー：標準入力の1行ごとに JSON-RPC のメッセージを読み、
-- 答えを標準出力に書く
module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BL
import System.IO (BufferMode (..), hSetBuffering, stdout)

import Modern.Mcp (handleLine)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  input <- BL.getContents
  mapM_ answer (BL.lines input)
  where
    answer line = mapM_ BL.putStrLn (handleLine line)
-- ANCHOR_END: all
