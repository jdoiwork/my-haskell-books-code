-- ANCHOR: all
-- | Measure: trying every place, against looking first, on the
--   archive (10000 quiet lines, and the page of the old records at the
--   end). A prefix is looked for with Text's search, and with KMP.
-- 測る：全部の場所を試すのと、先に見るのを、記録の山（静かな行が
-- 1万行と、最後に古い記録の1ページ）で。先頭の文字列は、Text の検索と
-- KMP の両方で探す
module Main (main) where

-- ANCHOR: imports
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Test.Tasty.Bench

import MiniRegex
import MiniRegex.Cursor (cursorAt)
import MiniRegex.Examples (archive, byKmp, everywhere, surely)
import MiniRegex.Prefilter (kmpPlaces, textPlaces)
-- ANCHOR_END: imports

-- ANCHOR: main
-- ⏱️ Each pattern, with and without looking first. The first match of
--    each is near the end of the archive (or there is none), so the
--    whole archive is searched.
-- パターンごとに、先に見るときと見ないときで。どのパターンも、最初の
-- 一致は記録の山の終わり近く（か、ない）なので、山の全部を探す
main :: IO ()
main = defaultMain
  [ race "room" "Room (\\d+)" True
  , race "dates" "\\b\\d{4}-\\d{2}-\\d{2}\\b" False
  , race "dragon" "(?i)dragon" False
  , race "mail" "(\\w+)@(\\w+)\\.com" False
  , bgroup "search"
      [ bench "text" (nf (length . textPlaces "Room ") start)
      , bcompare (against "text" "search")
          (bench "kmp" (nf (length . kmpPlaces "Room ") start))
      ]
  ]
  where
    start = fromMaybe (error "no cursor") (cursorAt book 0)

-- 📚 The archive to search.
-- 探す記録の山
book :: Text
book = archive 10000

-- 🏁 Every place, and looking first (and with KMP, if the pattern has a
--    prefix). The times are shown as multiples of every place's.
-- 全部の場所と、先に見るもの（先頭の文字列があれば KMP でも）。時間は、
-- 全部の場所の何倍かでも見せる
race :: String -> Text -> Bool -> Benchmark
race name p prefix = bgroup name $
  [ bench "every place" (run (everywhere re))
  , bcompare (against "every place" name) (bench "looking first" (run re))
  ] ++
  [ bcompare (against "every place" name) (bench "with kmp" (run (byKmp re)))
  | prefix ]
  where
    re = surely p
    run r = nf (fmap matchEnd . find r) book

-- 🎯 The benchmark to compare with.
-- くらべる相手のベンチマーク
against :: String -> String -> String
against base name =
  "$NF == \"" ++ base ++ "\" && $(NF-1) == \"" ++ name ++ "\""
-- ANCHOR_END: main
-- ANCHOR_END: all
