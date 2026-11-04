module Main (main) where

import Data.Char       (isAlpha, toLower)
import Data.List       (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord        (Down (..))

-- ANCHOR: body
main :: IO ()
main = do
  input <- getContents

  let wordList = splitIntoWords input
      counts   = countWords wordList
      ranking  = topN 10 counts

  mapM_ printEntry ranking

splitIntoWords :: String -> [String]
splitIntoWords text =
  let lettersOnly = map replaceNonLetter text
      lowered     = map toLower lettersOnly
  in  words lowered

replaceNonLetter :: Char -> Char
replaceNonLetter c
  | isAlpha c = c
  | otherwise = ' '

countWords :: [String] -> Map String Int
countWords wordList =
  let pairs = [ (w, 1) | w <- wordList ]
  in  Map.fromListWith (+) pairs

topN :: Int -> Map String Int -> [(String, Int)]
topN n counts =
  let pairs  = Map.toList counts
      sorted = sortOn countDescending pairs
  in  take n sorted
  where
    countDescending (_, count) = Down count

printEntry :: (String, Int) -> IO ()
printEntry (word, count) =
  putStrLn (padRight 12 word ++ show count)

padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '
-- ANCHOR_END: body
