-- ANCHOR: all
-- ANCHOR: header
-- | The jobs of the record room, written once against the classes of
--   regex-base. The engine is a type: datesIn @TDFA.Regex runs them
--   with regex-tdfa, datesIn @MiniRegex.Regex with MiniRegex.
-- 記録室の仕事を、regex-base のクラスだけを使って1度だけ書く。
-- エンジンは型で選ぶ：datesIn @TDFA.Regex なら regex-tdfa で、
-- datesIn @MiniRegex.Regex なら MiniRegex で動く
{-# LANGUAGE AllowAmbiguousTypes #-}
module MiniRegex.Portable
  ( Backend
  , datesIn
  , countGold
  , salesIn
  , firstWord
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Data.Array ((!))
import Data.Text (Text)
import Data.Text qualified as T

import Text.Regex.Base
-- ANCHOR_END: imports

-- ANCHOR: engine
-- 🧩 What a job asks of an engine (a backend, in regex-base's words):
--    it can make a regex from a String, and find in a Text.
-- 仕事がエンジン（regex-base の言葉では、バックエンド）に求めること：
-- String から正規表現を作れて、Text の中を探せる
type Backend r c e = (RegexMaker r c e String, RegexLike r Text)

-- 🔨 Make the regex of the engine r (a wrong pattern here is a bug of
--    the book, so error is fine).
-- エンジン r の正規表現を作る（ここでパターンがまちがっていたら、本の
-- バグなので、error でよい）
regexOf :: forall r c e. Backend r c e => String -> r
regexOf = makeRegex
-- ANCHOR_END: engine

-- ANCHOR: jobs
-- 📅 Every date in the book.
-- 帳面の日付を全部
datesIn :: forall r c e. Backend r c e => Text -> [Text]
datesIn book = map whole (matchAllText dates book)
  where
    dates = regexOf @r "[0-9]{4}-[0-9]{2}-[0-9]{2}"

-- 💰 How much gold was paid, in all ("for 370 gold": group 1).
-- 払われた金貨の合計（「for 370 gold」の1番のグループ）
countGold :: forall r c e. Backend r c e => Text -> Int
countGold book = sum (map amount (matchAllText gold book))
  where
    gold = regexOf @r "for ([0-9]+) gold"
    amount m = read (T.unpack (fst (m ! 1)))

-- 🧾 Who sold what: (the seller, the thing).
-- だれが何を売ったか：（売った人、売ったもの）
salesIn :: forall r c e. Backend r c e => Text -> [(Text, Text)]
salesIn book = map pair (matchAllText sold book)
  where
    sold = regexOf @r "([A-Z][A-Za-z]+) sold [0-9]+ ([a-z]+)"
    pair m = (fst (m ! 1), fst (m ! 2))

-- 🔤 The first of the words in the pattern that is in the text
--    (gold|golden: Perl takes the left one first, POSIX the longest).
-- パターンの単語のうち、文字列にある最初のもの（gold|golden：Perl は
-- 左から、POSIX はいちばん長いものを選ぶ）
firstWord :: forall r c e. Backend r c e => String -> Text -> Maybe Text
firstWord pattern t = case matchOnceText (regexOf @r pattern) t of
  Just (_, m, _) -> Just (whole m)
  Nothing        -> Nothing

-- 🎯 The text of the whole match (group 0).
-- 一致の全体（0番のグループ）の文字列
whole :: MatchText Text -> Text
whole m = fst (m ! 0)
-- ANCHOR_END: jobs
-- ANCHOR_END: all
