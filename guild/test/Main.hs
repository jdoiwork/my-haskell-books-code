-- ANCHOR: all
module Main (main) where

import Data.Maybe (isJust, isNothing)
import System.Exit (exitFailure)

import Guild.Gold (Gold, goldAmount, mkGold)

-- ✅ Each test: a name and whether it passed.
-- テスト1つ：名前と、合格したかどうか
tests :: [(String, Bool)]
tests =
  [ ("negative gold is rejected", isNothing (mkGold (-5)))
  , ("zero gold is fine",         isJust (mkGold 0))
  , ("30 gold is 30",             amountOf (mkGold 30) == 30)
  ]

-- 🔢 The amount inside, or -1 if there is no gold.
-- 中のゴールドの量。ゴールドがなければ -1
amountOf :: Maybe Gold -> Int
amountOf (Just g) = goldAmount g
amountOf Nothing  = -1

-- 🧪 Run all the tests. If one fails, the exit code says so.
-- すべてのテストを実行する。1つでも失敗したら、終了コードで知らせる
main :: IO ()
main = do
  mapM_ report tests
  if all snd tests
    then putStrLn "All tests passed."
    else exitFailure
  where
    report (name, ok) = putStrLn ((if ok then "ok   " else "FAIL ") ++ name)
-- ANCHOR_END: all
