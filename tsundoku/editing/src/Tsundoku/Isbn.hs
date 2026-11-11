-- ANCHOR: all
-- ANCHOR: header
-- | ISBNs, checked by their last digit. An Isbn is always one whose
--   check digit is right; the only way to make one is `parseIsbn`.
-- ISBN。最後の1けた（チェックディジット）で確かめる。Isbn の値は、いつも
-- チェックディジットが合っている。作る道は parseIsbn だけ
module Tsundoku.Isbn
  ( Isbn
  , IsbnProblem (..)
  , parseIsbn
  , isbnText
  , isIsbn13
  , checkDigit10
  , checkDigit13
  ) where

import Data.Char (digitToInt, intToDigit, isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T
-- ANCHOR_END: header

-- ANCHOR: types
-- 📚 An ISBN: 10 digits (the old ones, until 2006) or 13 digits.
--    Only the digits are kept, without hyphens. The last character of
--    an ISBN-10 can be X (it means 10).
-- ISBN：10けた（2006年までの古いもの）か13けた。ハイフンを外した数字だけ
-- を持つ。ISBN-10 の最後の1文字は X のこともある（10 の意味）
data Isbn
  = Isbn10 Text
  | Isbn13 Text
  deriving (Show, Eq, Ord)

-- 🚫 Why it is not an ISBN.
-- ISBN でない理由
data IsbnProblem
  = WrongLength   Int        -- not 10 or 13 / 10けたでも13けたでもない
  | NotADigit     Char       -- a letter where a digit goes / 数字でない文字
  | WrongCheck    Char Char  -- right, written / 正しい1けた、書いた1けた
  | NotABookCode  Text       -- 13 digits not 978/979 / 978・979 でない13けた
  deriving (Show, Eq)
-- ANCHOR_END: types

-- ANCHOR: parse
-- 🔍 Read an ISBN as people write it: hyphens and spaces may be
--    anywhere ("978-4-10-101001-4", "4 10 101001 3"), and "ISBN" may
--    come first, as on the back of a book. 13 digits must start with
--    978 or 979: the second bar code on a Japanese book (192...) has the
--    same check digit, but is not an ISBN.
-- 人が書くとおりの ISBN を読む。ハイフンと空白は、どこにあってもよく、
-- 本の裏のとおり頭に「ISBN」があってもよい。13けたは 978 か 979 で始まる
-- （日本の本の2段目のバーコード（192…）も、同じ計算のチェックディジット
-- を持つが、ISBN ではない）
parseIsbn :: Text -> Either IsbnProblem Isbn
parseIsbn typed =
  case T.length plain of
    10 -> checked Isbn10 checkDigit10 (T.init plain) (T.last plain)
    13 | T.take 3 plain `notElem` ["978", "979"], T.all isDigit plain
          -> Left (NotABookCode (T.take 3 plain))
       | otherwise
          -> checked Isbn13 checkDigit13 (T.init plain) (T.last plain)
    n  -> Left (WrongLength n)
  where
    upper = T.toUpper (T.filter (\c -> c /= '-' && not (isSpace c)) typed)
    plain = maybe upper id (T.stripPrefix "ISBN" upper)

    checked make digit body written =
      case T.find (not . isDigit) body of
        Just c  -> Left (NotADigit c)
        Nothing
          | written == expected -> Right (make plain)
          | otherwise           -> Left (WrongCheck expected written)
          where
            expected = digit body
-- ANCHOR_END: parse

-- ANCHOR: digits
-- 🔢 The check digit of an ISBN-10: weigh the 9 digits by 10, 9, ..., 2,
--    and take what makes the sum a multiple of 11 (10 is written X).
-- ISBN-10 のチェックディジット：9けたに 10, 9, …, 2 の重みを掛けて足し、
-- 和を 11 の倍数にする数（10 なら X と書く）
checkDigit10 :: Text -> Char
checkDigit10 body =
  case (11 - total `mod` 11) `mod` 11 of
    10 -> 'X'
    d  -> intToDigit d
  where
    total = sum (zipWith (*) [10, 9 .. 2] (digits body))

-- 🔢 The check digit of an ISBN-13: weigh the 12 digits by 1, 3, 1, 3,
--    ..., and take what makes the sum a multiple of 10.
-- ISBN-13 のチェックディジット：12けたに 1, 3, 1, 3, … の重みを掛けて
-- 足し、和を 10 の倍数にする数
checkDigit13 :: Text -> Char
checkDigit13 body = intToDigit ((10 - total `mod` 10) `mod` 10)
  where
    total = sum (zipWith (*) (cycle [1, 3]) (digits body))

digits :: Text -> [Int]
digits = map digitToInt . T.unpack
-- ANCHOR_END: digits

-- ANCHOR: text
-- 🏷️ The digits, as they are kept. Is it an ISBN-13?
-- しまってある数字。ISBN-13 か？
isbnText :: Isbn -> Text
isbnText (Isbn10 t) = t
isbnText (Isbn13 t) = t

isIsbn13 :: Isbn -> Bool
isIsbn13 (Isbn10 _) = False
isIsbn13 (Isbn13 _) = True
-- ANCHOR_END: text
-- ANCHOR_END: all
