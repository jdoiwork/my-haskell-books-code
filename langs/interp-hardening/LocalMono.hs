-- ANCHOR: all
-- Haskell (GHC2024): label uses tag and has no signature, so
-- MonoLocalBinds keeps it to one type. GHC stops it.
-- Haskell（GHC2024）：label は tag を使い、注釈がないので、
-- MonoLocalBinds が1つの型にする。GHC が止める
module Main (main) where

tagged :: String -> Int -> String -> String
tagged tag n name = label n ++ " " ++ label name
  where
    label x = tag ++ ":" ++ show x

main :: IO ()
main = putStrLn (tagged "#" 3 "lam")
-- ANCHOR_END: all
