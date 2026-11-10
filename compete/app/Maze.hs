-- ANCHOR: all
-- 🗺️ Problem 4: the fewest steps through the dungeon (-1 if no way).
-- 問題4：ダンジョンを抜ける、いちばん少ない歩数（行けなければ -1）
module Main (main) where

import Data.ByteString.Char8 qualified as BS

import Algo (shortestPath)

main :: IO ()
main = do
  input <- BS.getContents
  case BS.lines input of
    (size : rows) | Just (h, _) <- BS.readInt size ->
      -- take h rows: a blank line at the end is not a row
      -- h 行だけ使う（最後の空行は、迷路の行ではない）
      print (maybe (-1) id (shortestPath (map BS.unpack (take h rows))))
    _ -> pure ()
-- ANCHOR_END: all
