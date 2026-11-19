-- ANCHOR: all
-- | Measure the three matchers again, after the cursor and the loops
--   are polished: the continuations, the backtracking VM with ST, and
--   the Pike VM. Each VM is run by itself (not chosen by find).
-- しおりと輪を磨いたあとで、3つのマッチャーをもう一度測る：継続、ST の
-- バックトラックの VM、Pike VM。VM は、find に選ばせず、それぞれを動かす
module Main (main) where

-- ANCHOR: imports
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.Bench

import MiniRegex
import MiniRegex.Engine (Runner, searchCountingBy, searchFromCont)
import MiniRegex.Examples (surely)
import MiniRegex.Pike (scanPike)
import MiniRegex.STVM (scanVMST)
-- ANCHOR_END: imports

-- ANCHOR: main
-- ⏱️ The four of the chapter on the VM with ST, and a lookbehind far
--    from the head of the text.
-- ST の VM の章の4つと、文字列の頭から遠い後読み
main :: IO ()
main = defaultMain
  [ race "dates" "\\b\\d{4}-\\d{2}-\\d{2}\\b"
      (T.replicate 200 "Room 1234-56-789 is closed\n" <> "2026-11-19")
  , race "many-a" "a*b" (T.replicate 10000 "a" <> "b")
  , race "a-or-b" "(?:a|b)*c" (T.replicate 5000 "ab" <> "c")
  , race "names" "^(\\w+\\s?)+$" "Mira Ruby Lam GHC Perl!"
  , race "behind" "(?<=gold )\\d+" (T.replicate 20000 "x" <> "gold 12")
  ]

-- 🏁 The matchers, side by side. The VMs' times are shown as multiples
--    of the continuations'. The Pike VM cannot look behind.
-- マッチャーを並べる。VM の時間は、継続の何倍かでも見せる。Pike VM は
-- 後読みができない
race :: String -> Text -> Text -> Benchmark
race name p t = bgroup name $
  [ bench "continuation" (nf (\s -> fmap matchEnd (searchFromCont re s 0)) t)
  , bcompare against (bench "vm-st" (nf (fmap matchEnd . with scanVMST) t))
  ] ++
  [ bcompare against (bench "pike" (nf (fmap matchEnd . with scanPike) t))
  | name /= "behind" ]
  where
    re = surely p
    against =
      "$NF == \"continuation\" && $(NF-1) == \"" ++ name ++ "\""

    -- the first match with this VM, with find's limit
    -- この VM で最初の一致を。上限は find と同じ
    with :: Runner -> Text -> Maybe Match
    with vm s = case fst (searchCountingBy vm defaultLimit re s 0) of
      Right m -> m
      Left e  -> error (show e)
-- ANCHOR_END: main
-- ANCHOR_END: all
