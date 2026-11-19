module Twice where
class Describe a where
  describe :: a -> String
twice :: Describe a => a -> String
twice x = describe x ++ ", " ++ describe x
instance Describe Bool where
  describe b = if b then "yes" else "no"
yes :: String
yes = twice True
