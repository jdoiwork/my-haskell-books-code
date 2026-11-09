-- ANCHOR: all
-- | One model of a rating (a score from 0 to 100), and many ways to
--   show it: the views are only shadows of the model.
-- 評価のモデルは1つ（0〜100 の点）。見せ方はいくつもある。
-- ビューは、モデルの影にすぎない
-- ANCHOR: header
module Tsundoku.Rating
  ( -- * The model / モデル
    Score          -- no constructor: use `score` / コンストラクターは出さない
  , score
  , scoreValue
    -- * The views / ビュー
  , RatingStyle (..)
  , Decimals (..)
  , allStyles
  , render
  , parse
  , step
  , snap
  , marks
  ) where

-- reading numbers in text
-- 文字列の中の数を読む
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Tsundoku.Types (Checked, Mistake (..))
-- ANCHOR_END: header

-- ANCHOR: score
-- 💯 The model: a score from 0 to 100. Whatever the screen shows,
--    this is what the library keeps.
-- モデル：0〜100 の点。画面がどう見せても、図書室がしまうのはこれ
newtype Score = Score Int
  deriving (Show, Eq, Ord)

-- 🚪 The only way in: 0 to 100. Anything else is a mistake.
-- 点を作る、ただ1つの入り口：0〜100。それ以外はまちがい
score :: Int -> Checked Score
score n
  | n < 0 || n > 100 = Left (BadScore n)
  | otherwise        = Right (Score n)

-- 👀 Look inside (reading is fine; only making is guarded).
-- 中身を見る（読むのはよい。守るのは作るときだけ）
scoreValue :: Score -> Int
scoreValue (Score n) = n
-- ANCHOR_END: score

-- ANCHOR: style
-- 🎨 Ways to show a score. None of them is the score itself.
-- 点の見せ方。どれも、点そのものではない
data RatingStyle
  = Hundred             -- 73点
  | FiveStars           -- ★3.5 (by halves, 0 to 5 / 0〜5、0.5刻み)
  | OutOfTen Decimals   -- 7/10, 7.3/10
  deriving (Show, Eq)

-- 🔟 Out of ten: whole numbers only, or one decimal place.
-- 10点満点：整数だけか、小数1けたまでか
data Decimals
  = NoDecimals          -- 7/10
  | OneDecimal          -- 7.3/10, 8.0/10
  deriving (Show, Eq)

-- 📋 Every style, for tests and for a settings screen.
-- すべての見せ方（テストと、設定の画面のため）
allStyles :: [RatingStyle]
allStyles = [Hundred, FiveStars, OutOfTen NoDecimals, OutOfTen OneDecimal]

-- 📏 How many points one step of the style is worth.
-- その見せ方の1目盛りが、何点ぶんか
step :: RatingStyle -> Int
step style = case style of
  Hundred               -> 1    -- 73点
  FiveStars             -> 10   -- half a star / 半分の★
  OutOfTen NoDecimals   -> 10   -- 7/10
  OutOfTen OneDecimal   -> 1    -- 7.3/10
-- ANCHOR_END: style

-- ANCHOR: render
-- 🖼️ Show a score in a style. A coarse style rounds to its nearest
--    step, halves going up (75 is ★4, not ★3.5).
-- 点を、その見せ方で見せる。目盛りの粗い見せ方では、いちばん近い
-- 目盛りに丸める。ちょうど真ん中は切り上げ（75 点は★4。★3.5 ではない）
render :: RatingStyle -> Score -> Text
render style (Score n) = case style of
  Hundred             -> T.pack (show n) <> "点"
  FiveStars           -> "★" <> halves steps
  OutOfTen NoDecimals -> T.pack (show steps) <> "/10"
  OutOfTen OneDecimal -> oneDecimal n <> "/10"
  where
    -- how many steps of the style, rounded halves up
    -- 見せ方の目盛りがいくつか（真ん中は切り上げ）
    steps = (n + step style `div` 2) `div` step style

-- ⭐ Half stars as a number: 7 is "3.5", 8 is "4".
-- 半分の★の数を数に：7 は "3.5"、8 は "4"
halves :: Int -> Text
halves h = T.pack (show (h `div` 2)) <> (if odd h then ".5" else "")

-- 🔢 Tenths with one decimal place, always: 73 is "7.3", 80 is "8.0".
-- 0.1 の数を、いつも小数1けたで：73 は "7.3"、80 は "8.0"
oneDecimal :: Int -> Text
oneDecimal t = T.pack (show (t `div` 10) ++ "." ++ show (t `mod` 10))
-- ANCHOR_END: render

-- ANCHOR: parse
-- 📥 Read what someone typed in a style, back into the model.
--    The marks (点, ★, /10) may be left out.
-- その見せ方で打ちこまれたものを、モデルに読み戻す。
-- 印（点、★、/10）は、省いてもよい
parse :: RatingStyle -> Text -> Checked Score
parse style typed =
  case (style, readTenths number) of
    (Hundred, Just t)
      | t `mod` 10 == 0, t <= 1000 -> Right (Score (t `div` 10))
    (FiveStars, Just t)
      | t `mod` 5 == 0, t <= 50    -> Right (Score (t * 2))
    (OutOfTen NoDecimals, Just t)
      | t `mod` 10 == 0, t <= 100  -> Right (Score t)
    (OutOfTen OneDecimal, Just t)
      | t <= 100                   -> Right (Score t)
    _                              -> Left (BadRating typed)
  where
    number = T.strip (dropMark style (T.strip typed))

-- ✂️ Take off the style's own mark, if it is there.
-- その見せ方の印があれば、取る
dropMark :: RatingStyle -> Text -> Text
dropMark style text = fromMaybe text $ case style of
  Hundred    -> T.stripSuffix "点" text
  FiveStars  -> T.stripPrefix "★" text
  OutOfTen _ -> T.stripSuffix "/10" text

-- 🔢 "7" is 70 tenths, "7.3" is 73. Nothing else is a number here
--    (no minus sign, at most one decimal place).
-- "7" は 0.1 が 70 個、"7.3" は 73 個。それ以外は数と認めない
-- （マイナスなし、小数は1けたまで）
readTenths :: Text -> Maybe Int
readTenths text = case T.splitOn "." text of
  [whole]          | digits whole       -> Just (number whole * 10)
  [whole, decimal] | digits whole
                   , T.length decimal == 1
                   , digits decimal     -> Just (number whole * 10
                                                 + number decimal)
  _                                     -> Nothing
  where
    digits t = not (T.null t) && T.all isDigit t && T.length t <= 4
    number t = read (T.unpack t)
-- ANCHOR_END: parse

-- ANCHOR: snap
-- 🧲 The score a style really shows: the nearest step, halves going
--    up, as in `render` (73 is 70 in stars, because it shows ★3.5).
-- その見せ方で、本当に見えている点：いちばん近い目盛り。真ん中は
-- 切り上げ（render と同じ。73 点は、★なら 70 点。★3.5 と見えるから）
snap :: RatingStyle -> Score -> Score
snap style (Score n) = Score (steps * step style)
  where
    steps = (n + step style `div` 2) `div` step style

-- 📐 Every step of a style, from 0 to 100 (★0, ★0.5, ... ★5).
-- その見せ方の目盛りを全部、0 から 100 まで（★0、★0.5、……★5）
marks :: RatingStyle -> [Score]
marks style = map Score [0, step style .. 100]
-- ANCHOR_END: snap
-- ANCHOR_END: all
