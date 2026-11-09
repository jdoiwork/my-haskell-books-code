-- ANCHOR: all
-- ANCHOR: head
-- | Training 3: turn an ISBN-10 into an ISBN-13.
-- トレーニング3：ISBN-10 を ISBN-13 にする
module ShelfAnswers.Isbn13 (toIsbn13) where

import Data.Text qualified as T

import Tsundoku.Isbn
-- ANCHOR_END: head

-- ANCHOR: isbn13
-- 🔢 Put 978 in front, drop the old check digit, and work out the new
--    one. An ISBN-13 stays as it is.
-- 前に 978 を付け、古いチェックディジットを外して、新しいものを計算する。
-- ISBN-13 は、そのまま
toIsbn13 :: Isbn -> Isbn
toIsbn13 old
  | isIsbn13 old = old
  | otherwise    = case parseIsbn (body <> T.singleton (checkDigit13 body)) of
      Right new -> new
      Left  _   -> old   -- never: the digit is the right one / 起こらない
  where
    body = "978" <> T.take 9 (isbnText old)
-- ANCHOR_END: isbn13
-- ANCHOR_END: all
