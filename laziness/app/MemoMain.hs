-- ANCHOR: all
-- 🧱 Run the memo table with Data.Map.Strict, compiled.
-- Data.Map.Strict のメモの表を、コンパイルして動かす
module Main (main) where

import Memo (memoFibStrict)

main :: IO ()
main = print (memoFibStrict 30)
-- ANCHOR_END: all
