-- | This file does not compile with -Wall -Werror, on purpose:
--   Rescued was added, but describe was not updated.
-- わざとコンパイルが通らない（-Wall -Werror のとき）ファイル：
-- Rescued を足したのに、describe を直し忘れた
module Forgot where

-- ANCHOR: forgot
data Outcome
  = Cleared Int
  | Failed String
  | Fled
  | Rescued String
  deriving (Show, Eq)

describe :: Outcome -> String
describe (Cleared amount) = "cleared! +" <> show amount <> " G"
describe (Failed where_)  = "failed at " <> where_
describe Fled             = "ran away"
-- ANCHOR_END: forgot
