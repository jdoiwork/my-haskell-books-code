-- ANCHOR: all
-- | Count to ten million with a box, to see the memory with +RTS -s.
-- 箱で 1,000 万まで数えて、+RTS -s でメモリーを見る
module Main (main) where

import System.Environment (getArgs)

import Boxes  (countLazy, countSTLazy, countSTStrict, countStrict)
import Frozen (freezeDemo)

-- ANCHOR: main
main :: IO ()
main = do
  args <- getArgs
  let n = 10000000
  case args of
    ["lazy"]     -> countLazy n >>= print
    ["strict"]   -> countStrict n >>= print
    ["stlazy"]   -> print (countSTLazy n)
    ["ststrict"] -> print (countSTStrict n)
    ["freeze"]   -> freezeDemo >>= print
    -- 「使い方：boxes lazy|strict|stlazy|ststrict|freeze」
    _            -> putStrLn "usage: boxes lazy|strict|stlazy|ststrict|freeze"
-- ANCHOR_END: main
-- ANCHOR_END: all
