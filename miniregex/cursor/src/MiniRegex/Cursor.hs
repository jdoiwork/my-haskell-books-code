-- ANCHOR: all
-- | A cursor: where the matcher is in the text. It keeps the place
--   twice, counted in characters and in bytes, and the whole text. Text
--   keeps its characters as UTF-8 bytes in an array, so with the byte
--   place, the character ahead, the one behind, and what is ahead are
--   all read at once, without counting from the head.
-- しおり：マッチャーが文字列のどこにいるか。場所を、文字で数えたものと
-- バイトで数えたものの2つで持ち、文字列の全体も持つ。Text は文字を
-- UTF-8 のバイトの配列で持っているので、バイトの場所があれば、すぐ先の
-- 文字も、すぐ前の文字も、ここから先も、頭から数えずに、すぐ読める
module MiniRegex.Cursor
  ( Cursor
  , at
  , byte
  , whole
  , cursorAt
  , headOf
  , before
  , rest
  , step
  , onwards
  , taken
  , stepOver
  , stepOverBy
  , back
  , jumpTo
  ) where

-- ANCHOR: imports
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Unsafe (Iter (..), dropWord8, iter, lengthWord8,
                         reverseIter, takeWord8)
-- ANCHOR_END: imports

-- ANCHOR: cursor
-- 🔖 A place in the text: how many characters are behind, how many
--    bytes, and all of the text. Not a record with fields, and its
--    constructor stays in this module: a field could be used outside to
--    change one number and not the other (c { byte = 1 }), and the
--    cursor would no longer be at a character's edge.
-- 文字列の中の、ある場所：うしろに何文字あるか、何バイトあるか、文字列の
-- 全体。フィールドのあるレコードにはせず、構成子もこのモジュールの中だけ。
-- フィールドだと、外で片方の数だけを書き換えて（c { byte = 1 }）、文字の
-- 境目でないしおりを作れてしまう
data Cursor = Cursor !Int !Int !Text

-- 📏 What a cursor holds, read only.
-- しおりの持ち物。読むだけ
at, byte :: Cursor -> Int
at   (Cursor n _ _) = n
byte (Cursor _ b _) = b

whole :: Cursor -> Text
whole (Cursor _ _ t) = t

-- 🪞 Two cursors are the same when they are at the same place.
-- 2つのしおりは、同じ場所にあれば同じ
instance Eq Cursor where
  a == b = at a == at b && byte a == byte b

instance Show Cursor where
  show c = "Cursor {at = " ++ show (at c) ++ ", byte = " ++ show (byte c)
         ++ ", rest = " ++ show (rest c) ++ "}"

-- 🏁 The cursor at the head of the text.
-- 文字列の頭に置いたしおり
headOf :: Text -> Cursor
headOf = Cursor 0 0

-- 📍 A cursor at the n-th character (Nothing past the end). Only this
--    one counts from the head.
-- n 文字目に置いたしおり（終わりより先なら Nothing）。頭から数えるのは、
-- これだけ
cursorAt :: Text -> Int -> Maybe Cursor
cursorAt t n
  | n < 0 || n > T.length t = Nothing
  | otherwise = Just (Cursor n (T.measureOff n t) t)
-- ANCHOR_END: cursor

-- ANCHOR: read
-- The matchers call these three at every step: INLINE asks GHC to put
-- their bodies where they are called (the evening's measuring found it
-- worth it).
-- マッチャーは、この3つを1ステップごとに呼ぶ。INLINE は、呼ぶ所に中身を
-- 埋めこむよう GHC に頼む（夕方に測って、効き目を確かめた）

-- 👀 The character just behind (Nothing at the head).
-- すぐ前の文字（頭なら Nothing）
{-# INLINE before #-}
before :: Cursor -> Maybe Char
before (Cursor _ b t)
  | b == 0    = Nothing
  | otherwise = let Iter c _ = reverseIter t (b - 1) in Just c

-- 🔭 What is ahead: a slice of the same array, not a copy.
-- ここから先：同じ配列の切れ端で、写しではない
{-# INLINE rest #-}
rest :: Cursor -> Text
rest (Cursor _ b t) = dropWord8 b t

-- 👣 Read one character and move past it (Nothing at the end).
-- 1文字読んで、その先へ進む（終わりなら Nothing）
{-# INLINE step #-}
step :: Cursor -> Maybe (Char, Cursor)
step (Cursor n b t)
  | b >= lengthWord8 t = Nothing
  | otherwise = let Iter c d = iter t b in Just (c, Cursor (n + 1) (b + d) t)
-- ANCHOR_END: read

-- ANCHOR: walk
-- 🚶 This cursor, and every one after it, up to the end (the end, too).
-- このしおりと、そのあとの全部。終わりの場所も入る
onwards :: Cursor -> [Cursor]
onwards c = c : maybe [] (onwards . snd) (step c)

-- ✂️ The characters from one cursor up to a later one: a slice, too.
-- しおりから、あとのしおりまでの文字。これも切れ端
taken :: Cursor -> Cursor -> Text
taken from to = takeWord8 (byte to - byte from) (rest from)

-- 🦘 A later place in the same text, n characters and k bytes ahead.
--    The caller makes the promise: the k bytes ahead are n whole
--    characters (stepOver and textPlaces count them from a Text).
-- 同じ文字列の、n 文字・k バイト先の場所。呼ぶ側が約束する：この先の
-- k バイトは、ちょうど n 文字（stepOver と textPlaces は、Text から
-- 数えた数を渡す）
jumpTo :: Int -> Int -> Cursor -> Cursor
jumpTo n k (Cursor a b t) = Cursor (a + n) (b + k) t
-- ANCHOR_END: walk

-- ANCHOR: over
-- 🦘 If the text ahead starts with these characters, move past them
--    all (for \1: the same text again).
-- ここから先が、この文字列で始まっていたら、その全部を飛び越える
-- （\1 のため：同じ文字列を、もう一度）
stepOver :: Text -> Cursor -> Maybe Cursor
stepOver t c
  | t `T.isPrefixOf` rest c = Just (jumpTo (T.length t) (lengthWord8 t) c)
  | otherwise               = Nothing

-- 🦘 The same, but comparing each pair of characters with `same` (for
--    (?i)\1: a and A are the same). The two may be of different
--    lengths in bytes, so it steps one character at a time.
-- 同じことを、文字を1つずつ same でくらべながら（(?i)\1 のため：
-- a と A は同じ）。バイトの長さがちがうこともあるので、1文字ずつ進む
stepOverBy :: (Char -> Char -> Bool) -> Text -> Cursor -> Maybe Cursor
stepOverBy same t c = case T.uncons t of
  Nothing      -> Just c
  Just (x, t') -> case step c of
    Just (y, c') | same x y -> stepOverBy same t' c'
    _                       -> Nothing
-- ANCHOR_END: over

-- ANCHOR: back
-- 🔙 The cursor n characters back (Nothing before the start). It reads
--    the characters behind, one by one, from where it is: it costs as
--    much as n, however far the place is from the head.
-- n 文字前に戻したしおり（頭より前なら Nothing）。いまの場所から、
-- 後ろの文字を1つずつ読む。手間は n の分だけで、頭からの遠さによらない
back :: Int -> Cursor -> Maybe Cursor
back 0 c = Just c
back n (Cursor a b t)
  | b == 0    = Nothing
  | otherwise = let Iter _ d = reverseIter t (b - 1)
                in back (n - 1) (Cursor (a - 1) (b + d) t)
-- ANCHOR_END: back
-- ANCHOR_END: all
