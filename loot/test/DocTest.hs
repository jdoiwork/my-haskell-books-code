-- ANCHOR: all
-- 📚 Run the examples written in the comments (-- >>>) as tests.
-- コメントに書いた例（-- >>>）を、テストとして実行する
module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["-isrc", "src/Loot.hs", "src/Bag.hs", "src/Streak.hs"]
-- ANCHOR_END: all
