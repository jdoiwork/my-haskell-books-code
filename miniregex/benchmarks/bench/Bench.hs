-- ANCHOR: all
-- | Measure the ways to choose an engine, after this chapter's work:
--   the backtracking VM with ST, the Pike VM with ST, switching as the
--   chapter on switching engines wrote it, switching that goes on from
--   where the budget ran out, a budget for each place, and the bounded
--   backtracker (watching takeall only at its door, and at each
--   character; for a short text). Each one is run by itself (not chosen
--   by find).
-- この章の仕事のあとで、エンジンの選び方を測る：ST のバックトラックの
-- VM、ST の Pike VM、エンジンを乗り換える章の乗り換え、予算が尽きた
-- 場所から続ける乗り換え、場所ごとの予算、通った所を覚える
-- バックトラック（takeall を入り口だけで見張るものと、1文字ごとに
-- 見張るもの。短い文字列なら）。find に選ばせず、それぞれを動かす
module Main (main) where

-- ANCHOR: imports
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.Bench

import MiniRegex
import MiniRegex.Engine
  (Runner, scanShortFirst, scanSwitching, searchAtBy, searchCountingBy)
import MiniRegex.Cursor (headOf)
import MiniRegex.Examples
  (scanSwitchingAgain, scanSwitchingEach, surely, tallyOf)
import MiniRegex.Bounded (scanBoundedAtTheDoor)
import MiniRegex.PikeST (scanPikeST)
import MiniRegex.STVM (scanVMST)
-- ANCHOR_END: imports

-- ANCHOR: main
-- ⏱️ The six of the chapter on switching engines, and long words: two
--    hundred words of a hundred letters, and an address at the end.
--    Then findAll on a long tally, with Bounded.
-- エンジンを乗り換える章の6つと、長い単語：100 文字の単語が 200 個、
-- 最後に宛先。それから、長い集計に Bounded で findAll
main :: IO ()
main = defaultMain
  [ race "dates" "\\b\\d{4}-\\d{2}-\\d{2}\\b"
      (T.replicate 200 "Room 1234-56-789 is closed\n" <> "2026-11-19")
  , race "many-a" "a*b" (T.replicate 10000 "a" <> "b")
  , race "a-or-b" "(?:a|b)*c" (T.replicate 5000 "ab" <> "c")
  , race "names" "^(\\w+\\s?)+$" "Mira Ruby Lam GHC Perl!"
  , race "gold" "\\d+ gold" (T.replicate 2000 "12345 " <> "7 gold")
  , race "words" "\\w+@\\w+" (T.replicate 2000 "Mira Ruby " <> "lam@ghc")
  , race "long-words" "\\w+@\\w+"
      (T.replicate 200 (T.replicate 100 "a" <> " ") <> "lam@ghc")
  , walking
  ]

-- 🚶 findAll on a long tally: with switching, and with Bounded (as if
--    findAll did not switch for the walk).
-- 長い集計に findAll：乗り換えで、それから Bounded で（findAll が、
-- 歩くときに乗り換えに変えなかったとしたら）
walking :: Benchmark
walking = bgroup "findAll"
  [ bench "switch" (nf (length . findAll digits) t)
  , bcompare "$NF == \"switch\" && $(NF-1) == \"findAll\""
      (bench "bounded" (nf (length . boundedAll) t))
  ]
  where
    digits = surely "\\d+"
    t = tallyOf 20000
    -- every search with Bounded, from where the last match ended (the
    -- matches of \\d+ are never empty)
    -- どの探しものも Bounded で。前の一致の終わりから（\\d+ の一致は、
    -- 空にならない）
    boundedAll s = go (headOf s)
      where
        re = digits { engine = Bounded }
        go c = case fst (searchAtBy scanShortFirst defaultLimit re c) of
          Right (Just (m, end)) -> m : go end
          Right Nothing         -> []
          Left e                -> error (show e)

-- 🏁 The ways side by side, as multiples of the backtracking VM.
-- 選び方を並べる。バックトラックの VM の何倍かで
race :: String -> Text -> Text -> Benchmark
race name p t = bgroup name
  [ bench "vm-st" (run scanVMST)
  , vs (bench "pike-st" (run scanPikeST))
  , vs (bench "switch0" (run scanSwitchingAgain))
  , vs (bench "switch"  (run scanSwitching))
  , vs (bench "each"    (run (scanSwitchingEach 2 64)))
  , vs (bench "door"    (run scanBoundedAtTheDoor))
  , vs (bench "bounded" (run scanShortFirst))
  ]
  where
    re = surely p
    vs = bcompare ("$NF == \"vm-st\" && $(NF-1) == \"" ++ name ++ "\"")
    run vm = nf (fmap matchEnd . with vm) t

    -- the first match with this VM, with find's limit
    -- この VM で最初の一致を。上限は find と同じ
    with :: Runner -> Text -> Maybe Match
    with vm s = case fst (searchCountingBy vm defaultLimit re s 0) of
      Right m -> m
      Left e  -> error (show e)
-- ANCHOR_END: main
-- ANCHOR_END: all
