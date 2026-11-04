module Main (main) where

import Data.Char       (isAlpha, toLower)
import Data.List       (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord        (Down (..))

-- ANCHOR: body
main :: IO ()
main =
  interact $
    unlines
      . map (\(w, n) -> w ++ replicate (12 - length w) ' ' ++ show n)
      . take 10
      . sortOn (Down . snd)
      . Map.toList
      . Map.fromListWith (+)
      . map (, 1 :: Int)
      . words
      . map toLower
      . map (\c -> if isAlpha c then c else ' ')
-- ANCHOR_END: body
