-- ANCHOR: all
-- | Measure the engines again, after this chapter's work: the
--   backtracking VM with ST (with and without the possessive loops),
--   the Pike VM of the linear-time chapter, and the Pike VM with ST.
--   Each VM is run by itself (not chosen by find).
-- この章の仕事のあとで、エンジンをもう一度測る：ST のバックトラックの
-- VM（所有的な輪のあり・なし）、線形時間の章の Pike VM、ST の Pike VM。
-- VM は、find に選ばせず、それぞれを動かす
module Main (main) where

-- ANCHOR: imports
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.Bench

import MiniRegex
import MiniRegex.Engine (Runner, scanSwitching, searchCountingBy)
import MiniRegex.Examples (findAllSlowly, surely, tallyOf, unpossessed)
import MiniRegex.Pike (scanPike)
import MiniRegex.PikeST (scanPikeST)
import MiniRegex.STVM (scanVMST)
-- ANCHOR_END: imports

-- ANCHOR: main
-- ⏱️ The four of the chapter on the VM with ST, and two more: numbers
--    not followed by " gold", again and again, and a page of words with
--    an address at the end. Then findAll on a long tally.
-- ST の VM の章の4つと、もう2つ：" gold" が続かない数が何度も出てくる
-- もの、単語の並ぶページの最後に宛先があるもの。それから、長い集計に
-- findAll
main :: IO ()
main = defaultMain
  [ race "dates" "\\b\\d{4}-\\d{2}-\\d{2}\\b"
      (T.replicate 200 "Room 1234-56-789 is closed\n" <> "2026-11-19")
  , race "many-a" "a*b" (T.replicate 10000 "a" <> "b")
  , race "a-or-b" "(?:a|b)*c" (T.replicate 5000 "ab" <> "c")
  , race "names" "^(\\w+\\s?)+$" "Mira Ruby Lam GHC Perl!"
  , race "gold" "\\d+ gold" (T.replicate 2000 "12345 " <> "7 gold")
  , race "words" "\\w+@\\w+" (T.replicate 2000 "Mira Ruby " <> "lam@ghc")
  , walking
  ]

-- 🚶 findAll on a long tally, walking as the chapter before did (from
--    a number each time) and with a cursor.
-- 長い集計に findAll。前の章の歩き方（毎回、数から）と、しおりで
walking :: Benchmark
walking = bgroup "findAll"
  [ bench "slowly" (nf (length . findAllSlowly digits) t)
  , bcompare "$NF == \"slowly\" && $(NF-1) == \"findAll\""
      (bench "cursor" (nf (length . findAll digits) t))
  ]
  where
    digits = surely "\\d+"
    t = tallyOf 20000

-- 🏁 The engines, side by side, as multiples of the backtracking VM
--    with the program of the chapter before (no possessive loops).
-- エンジンを並べる。前の章のプログラム（所有的な輪なし）の
-- バックトラックの VM の何倍かで
race :: String -> Text -> Text -> Benchmark
race name p t = bgroup name
  [ bench "vm-st-plain" (run scanVMST (unpossessed re))
  , vs (bench "vm-st"   (run scanVMST re))
  , vs (bench "pike"    (run scanPike re))
  , vs (bench "pike-st" (run scanPikeST re))
  , vs (bench "switch"  (run scanSwitching re))
  ]
  where
    re = surely p
    vs = bcompare ("$NF == \"vm-st-plain\" && $(NF-1) == \"" ++ name ++ "\"")
    run vm r = nf (fmap matchEnd . with vm r) t

    -- the first match with this VM, with find's limit
    -- この VM で最初の一致を。上限は find と同じ
    with :: Runner -> Regex -> Text -> Maybe Match
    with vm r s = case fst (searchCountingBy vm defaultLimit r s 0) of
      Right m -> m
      Left e  -> error (show e)
-- ANCHOR_END: main
-- ANCHOR_END: all
