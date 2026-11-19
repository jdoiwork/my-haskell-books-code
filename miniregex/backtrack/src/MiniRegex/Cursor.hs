-- ANCHOR: all
-- | A cursor: where the matcher is in the text. It keeps the place
--   (counted in characters), the character just before it, and the
--   rest of the text, so that each step forward costs the same, however
--   long the text is.
-- しおり：マッチャーが文字列のどこにいるか。場所（文字で数える）と、
-- すぐ前の文字と、残りの文字列を持つ。文字列がどれだけ長くても、
-- 1歩進むのにかかる手間は同じ
module MiniRegex.Cursor
  ( Cursor (..)
  , cursorAt
  , step
  , onwards
  , taken
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
  }
  deriving (Show, Eq)

-- 📍 A cursor at the n-th character (Nothing past the end).
-- n 文字目に置いたしおり（終わりより先なら Nothing）
cursorAt :: Text -> Int -> Maybe Cursor
cursorAt t n
  | n < 0 || n > T.length t = Nothing
  | otherwise = Just (Cursor n (lastChar behind) ahead)
  where
    (behind, ahead) = T.splitAt n t
    lastChar s
      | T.null s  = Nothing
      | otherwise = Just (T.last s)

-- 👣 Read one character and move past it (Nothing at the end).
-- 1文字読んで、その先へ進む（終わりなら Nothing）
step :: Cursor -> Maybe (Char, Cursor)
step (Cursor n _ t) = case T.uncons t of
  Nothing      -> Nothing
  Just (c, t') -> Just (c, Cursor (n + 1) (Just c) t')

-- 🚶 This cursor, and every one after it, up to the end (the end, too).
-- このしおりと、そのあとの全部。終わりの場所も入る
onwards :: Cursor -> [Cursor]
onwards c = c : maybe [] (onwards . snd) (step c)

-- ✂️ The characters from one cursor up to a later one.
-- しおりから、あとのしおりまでの文字
taken :: Cursor -> Cursor -> Text
taken from to = T.take (at to - at from) (rest from)
-- ANCHOR_END: cursor
-- ANCHOR_END: all
