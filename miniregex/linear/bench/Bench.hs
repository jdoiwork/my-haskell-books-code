-- ANCHOR: all
-- | Measure: the backtracking VM (with ST) and the Pike VM, on the same
--   patterns and texts.
-- 測る：バックトラックの VM（ST のもの）と Pike VM を、同じパターンと
-- 文字列で
module Main (main) where

-- ANCHOR: imports
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.Bench

import MiniRegex
import MiniRegex.Examples (backtracking, fieldOf, records, surely)
-- ANCHOR_END: imports

-- ANCHOR: main
-- ⏱️ Each pattern, on each text, with both engines. The patterns are
--    the ones of the chapters before, and the name field with 5 names.
-- パターンと文字列ごとに、2つのエンジンで。パターンは前の章までと同じ
-- ものと、名前が5つの名前の欄
main :: IO ()
main = defaultMain
  [ race "dates" "\\b\\d{4}-\\d{2}-\\d{2}\\b"
      (T.replicate 200 "Room 1234-56-789 is closed\n" <> records)
  , race "many-a" "a*b" (T.replicate 10000 "a" <> "b")
  , race "a-or-b" "(?:a|b)*c" (T.replicate 5000 "ab" <> "c")
  , race "names" "^(\\w+\\s?)+$" (fieldOf 5)
  ]

-- 🏁 The two engines, side by side. The Pike VM's time is shown as a
--    multiple of the backtracking VM's.
-- 2つのエンジンを並べる。Pike VM の時間は、バックトラックの VM の
-- 何倍かでも見せる
race :: String -> Text -> Text -> Benchmark
race name p t = bgroup name
  [ bench "backtracking" (nf (fmap matchEnd . find (backtracking re)) t)
  , bcompare against (bench "pike" (nf (fmap matchEnd . find re) t))
  ]
  where
    re = surely p
    against =
      "$NF == \"backtracking\" && $(NF-1) == \"" ++ name ++ "\""
-- ANCHOR_END: main
-- ANCHOR_END: all
