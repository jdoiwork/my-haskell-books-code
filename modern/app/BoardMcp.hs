-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | board-mcp: the quest board as an MCP server.
--   The file of the board is the first argument (board.json if none).
-- board-mcp：MCP サーバーになった依頼板。
-- 依頼板のファイルは、最初の引数（なければ board.json）
module Main (main) where

import System.Environment (getArgs)

import Modern.BoardMcp (boardTools)
import Modern.McpKit (serve)

main :: IO ()
main = do
  args <- getArgs
  let path = case args of
        file : _ -> file
        []       -> "board.json"
  serve "board-mcp" (boardTools path)
-- ANCHOR_END: all
