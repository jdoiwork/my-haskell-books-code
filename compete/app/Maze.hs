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
    (_ : rows) -> print (maybe (-1) id (shortestPath (map BS.unpack rows)))
    _          -> pure ()
-- ANCHOR_END: all
