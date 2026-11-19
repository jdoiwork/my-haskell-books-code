-- ANCHOR: all
-- | Measure: the continuation matcher (walking the tree), the pure VM
--   and the VM with ST, on the same patterns and texts.
-- 測る：継続のマッチャー（木を歩く）、純粋な VM、ST の VM を、同じ
-- パターンと文字列で
module Main (main) where

-- ANCHOR: imports
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.Bench

import MiniRegex
import MiniRegex.Engine (searchCountingBy, searchFromCont)
import MiniRegex.STVM (scanVMSTFresh)
import MiniRegex.VM (scanVM)
import MiniRegex.Examples (records, surely)
-- ANCHOR_END: imports

-- ANCHOR: main
-- ⏱️ Each pattern, on each text: the continuations, the pure VM, the
--    VM with ST that makes a new machine for each place, and the VM
--    with ST.
-- パターンと文字列ごとに：継続、純粋な VM、場所ごとに機械を作る ST の
-- VM、ST の VM
main :: IO ()
main = defaultMain
  [ race "dates" "\\b\\d{4}-\\d{2}-\\d{2}\\b"
      (T.replicate 200 "Room 1234-56-789 is closed\n" <> records)
  , race "many-a" "a*b" (T.replicate 10000 "a" <> "b")
  , race "a-or-b" "(?:a|b)*c" (T.replicate 5000 "ab" <> "c")
  , race "names" "^(\\w+\\s?)+$"
      "Mira Ruby Lam GHC Perl!"
  ]

-- 🏁 The matchers, side by side. Each VM's time is shown as a multiple
--    of the continuations'.
-- マッチャーを並べる。VM の時間は、継続の何倍かでも見せる
race :: String -> Text -> Text -> Benchmark
race name p t = bgroup name
  [ bench "continuation" (nf (\s -> fmap matchEnd (searchFromCont re s 0)) t)
  , bcompare against (bench "vm" (nf (fmap matchEnd . pureFind) t))
  , bcompare against (bench "vm-st-fresh" (nf (fmap matchEnd . freshFind) t))
  , bcompare against (bench "vm-st" (nf (fmap matchEnd . find re) t))
  ]
  where
    re = surely p
    against =
      "$NF == \"continuation\" && $(NF-1) == \"" ++ name ++ "\""
    -- The pure VM, and the ST one that makes a new machine for each
    -- place, with the same limit as find's.
    -- 純粋な VM と、場所ごとに機械を作る ST の VM。上限は find と同じ
    pureFind  = with scanVM
    freshFind = with scanVMSTFresh
    with vm s = case fst (searchCountingBy vm defaultLimit re s 0) of
      Right m -> m
      Left e  -> error (show e)
-- ANCHOR_END: main
-- ANCHOR_END: all
