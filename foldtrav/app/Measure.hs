-- ANCHOR: all
-- 📏 Measure the reports on [1 .. n]: one walk with the tuple, four walks,
--    and one walk with the strict Monoid.
--      cabal run -v0 foldtrav-measure -- tuple 10000000 +RTS -s -RTS
-- [1 .. n] で集計を測る：組で1周、4周、正格な Monoid で1周
module Main (main) where

import System.Environment (getArgs)

import Stats

main :: IO ()
main = do
  args <- getArgs
  case args of
    [how, n] -> do
      let prices = [1 .. read n] :: [Int]
      print $ case how of
        "tuple"  -> report prices
        "strict" -> reportStrict prices
        _        -> Report { count    = length prices
                           , total    = sum prices
                           , cheapest = minimum prices
                           , dearest  = maximum prices }
    _ -> putStrLn "usage: foldtrav-measure (tuple|four|strict) N"
-- ANCHOR_END: all
