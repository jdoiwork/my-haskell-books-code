-- ANCHOR: all
-- 📦 One kind of box per do: the outer do is IO, the inner do is a list.
-- 1つの do には1種類の箱：外の do は IO、中の do はリスト
module Mixed (combos) where

import Data.Maybe (maybeToList)

-- ANCHOR: combos
-- 📦 Read a line (IO), then make every combination (a list).
--    The Maybe is turned into a list first, with maybeToList:
--    Just 5 -> [5], Nothing -> [].
-- 1行読んで（IO）、すべての組み合わせを作る（リスト）。
-- Maybe は先に maybeToList でリストに変える：Just 5 -> [5]、Nothing -> []
combos :: [Int] -> Maybe Int -> IO [(Int, Int, String)]
combos xs my = do
  line <- getLine
  let triples = do
        x <- xs
        y <- maybeToList my
        pure (x, y, line)
  pure triples
-- ANCHOR_END: combos
-- ANCHOR_END: all
