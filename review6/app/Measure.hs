-- ANCHOR: all
-- | How long do 100 suggestions take, with and without the length check?
-- 「もしかして」100 回に、長さの確認あり・なしで、どれだけかかる？
module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.Map as Map
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import Text.Printf (printf)

import MiniSearch.Corpus (corpus, wordAt)
import MiniSearch.Index (buildIndex)
import MiniSearch.Types
import Review.Suggest (suggest, suggestNaive)

main :: IO ()
main = do
  index <- evaluate (force (buildIndex (corpus 20000 100)))
  -- 100 typos: drop the second letter of a known word.
  -- 打ちまちがい 100 個：知っている単語の2文字目を落とす
  let typos = [ T.take 1 w <> T.drop 2 w | i <- [0, 37 .. 3700]
              , let w = wordAt i, T.length w > 3 ]
  printf "tokens: %d, typos: %d\n"
    (Map.size (indexPostings index)) (length typos)
  mapM_ (run index typos) [("naive", suggestNaive), ("length check", suggest)]
  where
    run index typos (name, f) = do
      t0 <- getMonotonicTime
      found <- evaluate (length (filter (/= Nothing) (map (f index) typos)))
      t1 <- getMonotonicTime
      printf "%-13s %.2f s (%d suggestions)\n"
        (name :: String) (t1 - t0) found
-- ANCHOR_END: all
