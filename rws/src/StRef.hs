-- ANCHOR: all
-- ANCHOR: imports
-- | ST and State: two ways to "change" something inside a pure function.
-- ST と State：純粋な関数の中で、何かを「書き換える」2つのやり方
module StRef
  ( sumState
  , sumST
  ) where

import Control.Monad.ST
import Control.Monad.State.Strict (execState, modify')
import Data.STRef
-- ANCHOR_END: imports

-- ANCHOR: both
-- 📦 State: the "changing" total is a value passed from step to step.
-- State：「変わる」合計は、手順から手順へ渡される値
sumState :: [Int] -> Int
sumState xs = execState (mapM_ (\x -> modify' (+ x)) xs) 0

-- 🔧 ST: a real box in memory, changed in place, locked inside runST.
-- ST：メモリーの中の本物の箱を、その場で書き換える。runST の中に閉じ込める
sumST :: [Int] -> Int
sumST xs = runST $ do
  total <- newSTRef 0
  mapM_ (\x -> modifySTRef' total (+ x)) xs
  readSTRef total
-- ANCHOR_END: both
-- ANCHOR_END: all
