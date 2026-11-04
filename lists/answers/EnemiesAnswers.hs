-- ANCHOR: module
module EnemiesAnswers where

import Data.List (sort)
-- ANCHOR_END: module

-- ANCHOR: weakestThree
-- 🔽 The three weakest HP values.
-- いちばん弱い3体の HP
weakestThree :: [Int] -> [Int]
weakestThree hps = take 3 (sort hps)
-- ANCHOR_END: weakestThree

-- ANCHOR: myReverse
-- 🔁 Reverse a list by ourselves. (recursion)
-- リストを自分で逆順にする（再帰）
myReverse :: [Int] -> [Int]
myReverse []       = []
-- reverse the rest, then put x last
-- 残りを逆順にして、最後に x
myReverse (x : xs) = myReverse xs ++ [x]
-- ANCHOR_END: myReverse
