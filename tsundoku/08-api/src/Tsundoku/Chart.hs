-- ANCHOR: all
-- | Ways to show gathered ratings as text: averages in a style,
--   histograms, and ranking tables. Only views of the numbers.
-- 集めた評価を文字で見せる：見せ方に合わせた平均、ヒストグラム、
-- ランキングの表。数の見せ方だけ
-- ANCHOR: header
module Tsundoku.Chart
  ( showAverage
  , showShare
  , histogram
  , rankingLines
  ) where

-- dictionaries and text
-- 辞書と文字列
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Tsundoku.Card
import Tsundoku.Rating
import Tsundoku.Types (Work (..))
-- ANCHOR_END: header

-- ANCHOR: showAverage
-- 🖼️ An average in a style, with one decimal place:
--    "90.9点", "★4.5", "9.1/10". Halves go up.
-- 平均を、その見せ方で、小数1けたまで見せる。真ん中は切り上げ
showAverage :: RatingStyle -> Rational -> Text
showAverage style x = case style of
  Hundred    -> tenths x <> "点"
  FiveStars  -> "★" <> tenths (x / 20)      -- 20 points a star / ★1つ 20 点
  OutOfTen _ -> tenths (x / 10) <> "/10"

-- 🔢 A number with one decimal place: 4.545... is "4.5".
-- 小数1けたの数に：4.545... は "4.5"
tenths :: Rational -> Text
tenths y = T.pack (show (t `div` 10) ++ "." ++ show (t `mod` 10))
  where
    t = floor (y * 10 + 1 / 2) :: Integer

-- 💯 A share as a whole percent: 0.627 is "63%".
-- 割合を、整数の百分率に：0.627 は "63%"
showShare :: Double -> Text
showShare s = T.pack (show (floor (s * 100 + 0.5) :: Int)) <> "%"
-- ANCHOR_END: showAverage

-- ANCHOR: histogram
-- 📊 How the ratings spread, one line per step of the style, from the
--    highest rating down to the lowest: "★5   ▓▓▓▓▓▓▓▓▓▓ 18".
--    Steps in between with no ratings are shown too, as empty lines.
-- 評価の散らばり。見せ方の目盛りごとに1行、いちばん高い評価から
-- 低い評価まで。あいだの、評価のない目盛りも、空の行で見せる
histogram :: Int -> RatingStyle -> [Score] -> [Text]
histogram width style scores =
  [ T.justifyLeft labelWidth ' ' (render style m)
      <> " " <> T.justifyLeft width ' ' (cells (howMany m))
      <> " " <> T.pack (show (howMany m))
  | m <- reverse shown
  ]
  where
    counts  = Map.fromListWith (+) [ (snap style s, 1 :: Int) | s <- scores ]
    shown   = case (Map.lookupMin counts, Map.lookupMax counts) of
      (Just (lo, _), Just (hi, _)) ->
        [ m | m <- marks style, lo <= m, m <= hi ]
      _ -> []
    howMany m  = Map.findWithDefault 0 m counts
    most       = maximum (1 : Map.elems counts)
    -- cells rounded up, so one rating is never invisible
    -- マス目は切り上げ。1件でも、見えなくならない
    cells c    = T.replicate ((c * width + most - 1) `div` most) "▓"
    labelWidth = maximum (0 : map (T.length . render style) shown)
-- ANCHOR_END: histogram

-- ANCHOR: rankingLines
-- 🏆 A ranking as lines: "1. ★4.5  迷宮図書館の写本係（30件）".
--    How to show each key is up to the caller.
-- ランキングを行に。値の見せ方は、呼ぶ側が決める
rankingLines :: (k -> Text) -> [(k, Card)] -> [Text]
rankingLines shown ranked =
  [ T.pack (show place) <> ". " <> T.justifyRight keyWidth ' ' (shown k)
      <> "  " <> title (work card)
      <> "（" <> T.pack (show (length (ratingsOf card))) <> "件）"
  | (place, (k, card)) <- zip [1 :: Int ..] ranked
  ]
  where
    keyWidth = maximum (0 : [ T.length (shown k) | (k, _) <- ranked ])
-- ANCHOR_END: rankingLines
-- ANCHOR_END: all
