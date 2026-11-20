-- ANCHOR: all
-- ANCHOR: head
-- | The answers to the training of the regex-base chapter: finding in
--   a ByteString (places in bytes), and answers chosen by name.
-- regex-base の章のトレーニングの解答：ByteString の中を探す（場所は
-- バイト）と、答えを名前で選ぶ関数
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module RegexBaseAnswers
  ( matches
  , firstMatch
  , allMatches
  , countMatches
  ) where

import Data.Array (listArray, (!))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient)

import MiniRegex (Match (..), Span (..), byteOffset, findAll)
import MiniRegex.Portable (Backend)
import Text.Regex.Base.Impl (polymatch, polymatchM)
import Text.Regex.MiniRegex
-- ANCHOR_END: head

-- ANCHOR: bytes
-- 🧱 Step 2: find in a ByteString of UTF-8. regex-base cuts a
--    ByteString in bytes (B.take and B.drop), so the places must be
--    bytes too: count them with byteOffset.
-- ステップ2：UTF-8 の ByteString の中を探す。regex-base は ByteString を
-- バイトで切る（B.take と B.drop）ので、場所もバイトにする。byteOffset
-- で数え直す
instance RegexLike Regex ByteString where
  matchAll re bs = map (inBytes t) (findAll re t)
    where
      t = decodeUtf8Lenient bs
  -- matchOnce and matchOnceText are written with each other: give one
  -- matchOnce と matchOnceText は、おたがいを使って書いてあるので、片方を書く
  matchOnce re bs = case matchAll re bs of
    m : _ -> Just m
    []    -> Nothing

instance RegexContext Regex ByteString ByteString where
  match  = polymatch
  matchM = polymatchM

-- 📏 A Match, with the places in bytes of t.
-- Match を、t のバイトで数えた場所で
inBytes :: Text -> Match -> MatchArray
inBytes t m = listArray (0, length places - 1) places
  where
    places = span' (matchStart m) (matchEnd m)
           : map (maybe (-1, 0) (\s -> span' (spanStart s) (spanEnd s)))
                 (matchGroups m)
    span' from to = let b = byteOffset t from
                    in  (b, byteOffset t to - b)
-- ANCHOR_END: bytes

-- ANCHOR: named
-- 🏷️ Step 3: the answers of =~, chosen by name instead of by type.
--    Written against regex-base, so they work with any backend.
-- ステップ3：=~ の答えを、型ではなく名前で選ぶ。regex-base だけで
-- 書いたので、どのバックエンドでも動く

-- ✅ Is it there?
-- あるか
matches :: forall r c e. Backend r c e => String -> Text -> Bool
matches p = matchTest (makeRegex p :: r)

-- 🔍 The first match, or Nothing.
-- 最初に見つかったものか、Nothing
firstMatch :: forall r c e. Backend r c e => String -> Text -> Maybe Text
firstMatch p t = case matchOnceText (makeRegex p :: r) t of
  Just (_, m, _) -> Just (fst (m ! 0))
  Nothing        -> Nothing

-- 🔎 Every match.
-- 見つかったもの全部
allMatches :: forall r c e. Backend r c e => String -> Text -> [Text]
allMatches p t = [fst (m ! 0) | m <- matchAllText (makeRegex p :: r) t]

-- 🔢 How many.
-- いくつあるか
countMatches :: forall r c e. Backend r c e => String -> Text -> Int
countMatches p = matchCount (makeRegex p :: r)
-- ANCHOR_END: named
-- ANCHOR_END: all
