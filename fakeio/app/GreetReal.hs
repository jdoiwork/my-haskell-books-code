-- | greet with the real IO.
-- greet を、本物の IO で
module Main (main) where

import Programs (greet)

main :: IO ()
main = greet
