-- ANCHOR: all
-- | Count to n by keeping a log with Writer (lazy, strict and CPS) and
--   with strict State, to compare the memory they use.
--     cabal run rws-measure -- lazy 1000000 +RTS -s
-- Writer（遅延・正格・CPS）と正格な State で n まで数え、
-- 使うメモリーを比べる
module Main (main) where

import qualified Control.Monad.State.Strict as SS
import qualified Control.Monad.Writer.CPS as WC
import qualified Control.Monad.Writer.Lazy as WL
import qualified Control.Monad.Writer.Strict as WS
import Data.Monoid (Sum (..))
import System.Environment (getArgs)

-- One more in the count.
-- 数を1つ増やす
one :: Sum Int
one = Sum 1

main :: IO ()
main = do
  [mode, size] <- getArgs
  let n = read size :: Int
      ones = [1 .. n]
      count = case mode of
        "lazy"   -> getSum (WL.execWriter (mapM_ (\_ -> WL.tell one) ones))
        "strict" -> getSum (WS.execWriter (mapM_ (\_ -> WS.tell one) ones))
        "cps"    -> getSum (WC.execWriter (mapM_ (\_ -> WC.tell one) ones))
        _        -> SS.execState (mapM_ (\_ -> SS.modify' (+ 1)) ones) 0
  print (count :: Int)
-- ANCHOR_END: all
