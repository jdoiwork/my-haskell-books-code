-- ANCHOR: all
-- Haskell (GHC2024: MonoLocalBinds is on)
-- Haskell（GHC2024：MonoLocalBinds が入っている）
module Main (main) where

-- ANCHOR: closed
-- 🔒 cell uses no local name: it is generalized, as Lam does.
-- cell はローカルな名前を使わない。Lam と同じく、一般にする
row :: Int -> String -> String
row n name = cell n ++ " | " ++ cell name
  where
    cell x = "[" ++ show x ++ "]"
-- ANCHOR_END: closed

-- ANCHOR: annotated
-- ✍️ label uses tag: with its signature, it is general too.
-- label は tag を使う。型の注釈があれば、これも一般にする
tagged :: String -> Int -> String -> String
tagged tag n name = label n ++ " " ++ label name
  where
    label :: Show a => a -> String
    label x = tag ++ ":" ++ show x
-- ANCHOR_END: annotated

-- ANCHOR: group
-- 🔁 Definitions in any order; isEven and isOdd are one group.
-- 定義はどの順でもよい。isEven と isOdd は1つのグループ
main :: IO ()
main = do
  putStrLn (row 3 "lam")
  putStrLn (tagged "#" 3 "lam")
  print (isEven (10 :: Int), isOdd (7 :: Int))

isEven, isOdd :: Int -> Bool
isEven n = n == 0 || isOdd (n - 1)
isOdd n = n /= 0 && isEven (n - 1)
-- ANCHOR_END: group
-- ANCHOR_END: all
