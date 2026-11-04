-- ANCHOR: all
module Main (main) where

-- HasCallStack: "remember who called me".
-- HasCallStack：「誰に呼ばれたかを覚えておいて」
import GHC.Stack (HasCallStack)

-- 💸 Pay the price from the wallet. Not enough gold is a bug here.
-- 財布から代金を払う。ここでは、お金が足りないのはバグ
spend :: HasCallStack => Int -> Int -> Int
spend price wallet
  | price > wallet = error "not enough gold"
  | otherwise      = wallet - price

main :: IO ()
main = do
  print (spend 30 100)
  print (spend 500 100)
-- ANCHOR_END: all
