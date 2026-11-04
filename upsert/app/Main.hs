-- ANCHOR: all
module Main where

-- Map.upsert needs containers 0.8.1 or newer.
-- Map.upsert には containers 0.8.1 以上が必要
import qualified Data.Map.Strict as Map

-- ANCHOR: countOne
-- ➕ For Map.upsert: one more, or 1 if we had none.
-- Map.upsert 用：1つ増やす。持っていなければ 1
countOne :: Maybe Int -> Int
countOne Nothing  = 1
countOne (Just n) = n + 1
-- ANCHOR_END: countOne

-- 🚀 Add a tent we do not have, and a potion we already have.
-- 持っていないテントと、持っているポーションを1つずつ足す
main :: IO ()
main = do
  let bag = Map.fromList [("potion", 3), ("herb", 1)]
  print (Map.upsert countOne "tent" bag)
  print (Map.upsert countOne "potion" bag)
-- ANCHOR_END: all
