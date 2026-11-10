-- | A mistake: taking the result of runState with <- in IO.
-- まちがい：runState の答えを、IO の中で <- で受け取ろうとする
module Main (main) where

import Control.Monad.State (runState)

import Battle (fight, start)

-- ANCHOR: arrow
main :: IO ()
main = do
  result <- runState fight (start 42)
  print result
-- ANCHOR_END: arrow
