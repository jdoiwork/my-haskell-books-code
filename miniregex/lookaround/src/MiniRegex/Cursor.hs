-- ANCHOR: all
-- | A cursor: where the matcher is in the text. It keeps the place
--   (counted in characters), the character just before it, and the
--   rest of the text, so that each step forward costs the same, however
--   long the text is. It also keeps the whole text, so that a
--   lookbehind can step back.
-- しおり：マッチャーが文字列のどこにいるか。場所（文字で数える）と、
-- すぐ前の文字と、残りの文字列を持つ。文字列がどれだけ長くても、
-- 1歩進むのにかかる手間は同じ。後読みが後ろへ下がれるように、
-- 文字列の全体も持つ
module MiniRegex.Cursor
  ( Cursor (..)
  , cursorAt
  , step
  , onwards
  , taken
  , stepOver
  , stepOverBy
  , back
  ) where

-- ANCHOR: imports
import Data.Text (Text)
import Data.Text qualified as T
-- ANCHOR_END: imports

-- ANCHOR: cursor
-- 🔖 A place in the text.
-- 文字列の中の、ある場所
data Cursor = Cursor
  { at     :: !Int         -- how many characters are behind / 何文字目か
  , before :: !(Maybe Char) -- the one just behind / すぐ前の文字
  , rest   :: !Text        -- what is ahead / ここから先
  , whole  :: !Text        -- all of it / 文字列の全体
  }
  deriving (Show, Eq)

-- 📍 A cursor at the n-th character (Nothing past the end).
-- n 文字目に置いたしおり（終わりより先なら Nothing）
cursorAt :: Text -> Int -> Maybe Cursor
cursorAt t n
  | n < 0 || n > T.length t = Nothing
  | otherwise = Just (Cursor n (lastChar behind) ahead t)
  where
    (behind, ahead) = T.splitAt n t
    lastChar s
      | T.null s  = Nothing
      | otherwise = Just (T.last s)

-- 👣 Read one character and move past it (Nothing at the end).
-- 1文字読んで、その先へ進む（終わりなら Nothing）
step :: Cursor -> Maybe (Char, Cursor)
step (Cursor n _ t w) = case T.uncons t of
  Nothing      -> Nothing
  Just (c, t') -> Just (c, Cursor (n + 1) (Just c) t' w)

-- 🚶 This cursor, and every one after it, up to the end (the end, too).
-- このしおりと、そのあとの全部。終わりの場所も入る
onwards :: Cursor -> [Cursor]
onwards c = c : maybe [] (onwards . snd) (step c)

-- ✂️ The characters from one cursor up to a later one.
-- しおりから、あとのしおりまでの文字
taken :: Cursor -> Cursor -> Text
taken from to = T.take (at to - at from) (rest from)
-- ANCHOR_END: cursor

-- ANCHOR: over
-- 🦘 If the text ahead starts with these characters, move past them
--    all (for \1: the same text again).
-- ここから先が、この文字列で始まっていたら、その全部を飛び越える
-- （\1 のため：同じ文字列を、もう一度）
stepOver :: Text -> Cursor -> Maybe Cursor
stepOver t c
  | T.null t  = Just c
  | otherwise = do
      ahead <- T.stripPrefix t (rest c)
      pure c { at     = at c + T.length t
             , before = Just (T.last t)
             , rest   = ahead }

-- 🦘 The same, but comparing each pair of characters with `same` (for
--    (?i)\1: a and A are the same).
-- 同じことを、文字を1つずつ same でくらべながら（(?i)\1 のため：
-- a と A は同じ）
stepOverBy :: (Char -> Char -> Bool) -> Text -> Cursor -> Maybe Cursor
stepOverBy same t c
  | T.length ahead < n                    = Nothing
  | and (zipWith same (T.unpack t) (T.unpack (T.take n ahead))) =
      Just c { at = at c + n, before = lastOf t, rest = T.drop n ahead }
  | otherwise                             = Nothing
  where
    n = T.length t
    ahead = rest c
    lastOf s = if T.null s then before c else Just (T.last (T.take n ahead))
-- ANCHOR_END: over

-- ANCHOR: back
-- 🔙 The cursor n characters back (Nothing before the start). It
--    counts again from the head of the text, so it costs as much as
--    the place is far; the chapter on making it fast does better.
-- n 文字前に戻したしおり（頭より前なら Nothing）。文字列の頭から数え
-- 直すので、場所が遠いほど手間がかかる（速くする章で、よくする）
back :: Int -> Cursor -> Maybe Cursor
back n c = cursorAt (whole c) (at c - n)
-- ANCHOR_END: back
-- ANCHOR_END: all
