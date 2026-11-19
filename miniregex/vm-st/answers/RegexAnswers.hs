-- ANCHOR: all
-- ANCHOR: head
-- | The answers to the training: the design chapter's, and then the
--   chapter on the VM with ST's.
-- トレーニングの解答：設計の章のものと、ST の VM の章のもの
module RegexAnswers
  ( colourTree
  , choiceTree
  , escape
  , placesTried
  , Drawers
  , Trail
  , write
  , undoTopDown
  , undoBottomUp
  , CountedStack
  , newCounted
  , pushC
  , popC
  , sizeC
  ) where

import Control.Monad.ST (ST)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef)
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex
import MiniRegex.Stack
import MiniRegex.Syntax
-- ANCHOR_END: head

-- The design chapter's training
-- 設計の章のトレーニング

-- 🌈 colou?r: c, o, l, o, then u zero or one time, then r.
-- colou?r：c・o・l・o、u を 0 回か 1 回、そして r
colourTree :: Pattern
colourTree = Pattern (Branch pieces :| [])
  where
    pieces =
      [ Quantified (Lit 'c') once
      , Quantified (Lit 'o') once
      , Quantified (Lit 'l') once
      , Quantified (Lit 'o') once
      , Quantified (Lit 'u') optional
      , Quantified (Lit 'r') once
      ]

-- 🍀 (?:ab)+|c: two branches. The left one is a group that does not
--    keep what it found, one time or more.
-- (?:ab)+|c：枝が2本。左は、見つけたものを覚えないグループを1回以上
choiceTree :: Pattern
choiceTree = Pattern (left :| [right])
  where
    left  = Branch [Quantified (Group NonCapturing ab) plus]
    ab    = Pattern (Branch [ Quantified (Lit 'a') once
                            , Quantified (Lit 'b') once ] :| [])
    right = Branch [Quantified (Lit 'c') once]

-- 🛡️ Put \ before every character with a meaning in a pattern, so
--    that the pattern finds the text just as it is.
-- パターンの中で意味を持つ文字の前に \ を付けて、その文字列そのものを
-- 探すパターンにする
escape :: Text -> Text
escape = T.concatMap one
  where
    one c
      | c `elem` special = T.pack ['\\', c]
      | otherwise        = T.singleton c
    special = "\\^$.|?*+()[]{}" :: String

-- The training of the chapter on the VM with ST
-- ST の VM の章のトレーニング

-- ANCHOR: places
-- 📍 How many places the VM is started at, to search the text: every
--    place up to where the match starts, or every place (the end, too)
--    when there is none. For a pattern without \K.
-- 文字列を探すのに、VM を動かし始める場所の数：一致が始まる場所まで
-- 全部。一致がなければ、全部の場所（終わりの場所も）。\K のない
-- パターンで
placesTried :: Regex -> Text -> Int
placesTried re t = case find re t of
  Just m  -> matchStart m + 1
  Nothing -> T.length t + 1
-- ANCHOR_END: places

-- ANCHOR: trail
-- 🗄️ A model of the drawers, with no ST: a Map from the drawer's number
--    to what is in it.
-- 引き出しの模型。ST は使わない：引き出しの番号から、中身への Map
type Drawers = Map Int Char

-- 📝 The trail: what was in a drawer before a write. The top is first.
-- 取り消しの記録：書き換える前の、引き出しの中身。先頭がいちばん上
type Trail = [(Int, Maybe Char)]

-- ✍️ Write over a drawer, and put the old value on the trail.
-- 引き出しを書き換え、前の値を記録に積む
write :: Int -> Char -> (Drawers, Trail) -> (Drawers, Trail)
write k v (ds, tr) = (Map.insert k v ds, (k, Map.lookup k ds) : tr)

-- ⏪ Put the old values back, from the top down to height h (the
--    chapter's undoTo).
-- 上から、高さ h まで、前の値を書き戻す（章の undoTo）
undoTopDown :: Int -> (Drawers, Trail) -> (Drawers, Trail)
undoTopDown h (ds, tr) = (foldl putBack ds top, rest)
  where
    (top, rest) = splitAt (length tr - h) tr

-- ⏩ The same, but from the bottom up: the wrong order.
-- 同じことを、下から上へ：まちがった順番
undoBottomUp :: Int -> (Drawers, Trail) -> (Drawers, Trail)
undoBottomUp h (ds, tr) = (foldl putBack ds (reverse top), rest)
  where
    (top, rest) = splitAt (length tr - h) tr

-- ↩️ Put one old value back.
-- 前の値を1つ書き戻す
putBack :: Drawers -> (Int, Maybe Char) -> Drawers
putBack ds (k, Nothing) = Map.delete k ds
putBack ds (k, Just v)  = Map.insert k v ds
-- ANCHOR_END: trail

-- ANCHOR: counted
-- 🥞 A stack that keeps its own count, in an STRef, on top of the
--    chapter's Stack.
-- 数を自分で持つスタック。章の Stack の上に、数を STRef で持つ
data CountedStack s a = CountedStack (Stack s a) (STRef s Int)

newCounted :: ST s (CountedStack s a)
newCounted = CountedStack <$> newStack <*> newSTRef 0

-- ⬆️ Put one on.
-- 1つ積む
pushC :: CountedStack s a -> a -> ST s ()
pushC (CountedStack st n) x = do
  k <- readSTRef n
  _ <- push st k x
  modifySTRef' n (+ 1)

-- ⬇️ Take the top one off (Nothing when it is empty).
-- いちばん上を1つ下ろす（空なら Nothing）
popC :: CountedStack s a -> ST s (Maybe a)
popC (CountedStack st n) = do
  k <- readSTRef n
  if k == 0
    then pure Nothing
    else do
      modifySTRef' n (subtract 1)
      Just <$> peek st (k - 1)

-- 🔢 How many are on it.
-- いくつ積んであるか
sizeC :: CountedStack s a -> ST s Int
sizeC (CountedStack _ n) = readSTRef n
-- ANCHOR_END: counted
-- ANCHOR_END: all
