-- ANCHOR: all
-- | Measure: the continuation matcher (walking the tree) and the VM,
--   on the same patterns and texts.
-- 測る：継続のマッチャー（木を歩く）と VM を、同じパターンと文字列で
module Main (main) where

-- ANCHOR: imports
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.Bench

import MiniRegex
import MiniRegex.Engine (searchFromCont)
import MiniRegex.Examples (records, surely)
-- ANCHOR_END: imports

-- ANCHOR: main
-- ⏱️ Each pattern, on each text: the continuations, then the VM.
-- パターンと文字列ごとに：継続、そのあと VM
main :: IO ()
main = defaultMain
  [ race "dates" "\\b\\d{4}-\\d{2}-\\d{2}\\b"
      (T.replicate 200 "Room 1234-56-789 is closed\n" <> records)
  , race "many-a" "a*b" (T.replicate 10000 "a" <> "b")
  , race "a-or-b" "(?:a|b)*c" (T.replicate 5000 "ab" <> "c")
  , race "names" "^(\\w+\\s?)+$"
      "Mira Ruby Lam GHC Perl!"
  ]

-- 🏁 The two matchers, side by side. The VM's time is shown as a
--    multiple of the continuations'.
-- 2つのマッチャーを並べる。VM の時間は、継続の何倍かでも見せる
race :: String -> Text -> Text -> Benchmark
race name p t = bgroup name
  [ bench "continuation" (nf (\s -> fmap matchEnd (searchFromCont re s 0)) t)
  , bcompare ("$NF == \"continuation\" && $(NF-1) == \"" ++ name ++ "\"")
      (bench "vm" (nf (fmap matchEnd . find re) t))
  ]
  where
    re = surely p
-- ANCHOR_END: main
-- ANCHOR_END: all
