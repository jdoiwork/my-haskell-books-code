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
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe (listToMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Foreign (lengthWord8)

import MiniRegex (Match (..), Span (..), find, findAll)
import MiniRegex.Portable (Backend)
import Text.Regex.Base.Impl (polymatch, polymatchM)
import Text.Regex.MiniRegex
-- ANCHOR_END: head

-- ANCHOR: bytes
-- 🧱 Step 2: find in a ByteString of UTF-8. regex-base cuts a
--    ByteString in bytes (B.take and B.drop), so the places must be
--    bytes too.
-- ステップ2：UTF-8 の ByteString の中を探す。regex-base は ByteString を
-- バイトで切る（B.take と B.drop）ので、場所もバイトにする
instance RegexLike Regex ByteString where
  matchAll re bs = inBytes t (findAll re t)
    where
      t = utf8 bs
  -- matchOnce and matchOnceText are written with each other: give one
  -- (with find, which stops at the first match)
  -- matchOnce と matchOnceText は、おたがいを使って書いてあるので、片方を書く
  -- （最初の一致で止まる find で）
  matchOnce re bs = listToMaybe (inBytes t (maybeToList (find re t)))
    where
      t = utf8 bs

instance RegexContext Regex ByteString ByteString where
  match  = polymatch
  matchM = polymatchM

-- 🚫 Only UTF-8. A byte that is not UTF-8 is an error: turned into
--    U+FFFD (three bytes), it would move every place after it.
-- UTF-8 だけ。UTF-8 でないバイトは error にする。U+FFFD（3バイト）に
-- 置き換えると、そのうしろの場所が、みんなずれる
utf8 :: ByteString -> Text
utf8 bs = either (error . ("MiniRegex: not UTF-8: " <>) . show) id
                 (decodeUtf8' bs)

-- 📏 The matches, with the places in bytes of t. Every place is turned
--    into bytes in one walk over t, from the left.
-- Match を、t のバイトで数えた場所で。場所は全部まとめて、t を左から
-- 1度歩いてバイトに直す
inBytes :: Text -> [Match] -> [MatchArray]
inBytes t ms = map one ms
  where
    one m = listArray (0, length places - 1) places
      where
        places = span' (matchStart m) (matchEnd m)
               : map (maybe (-1, 0) (\s -> span' (spanStart s) (spanEnd s)))
                     (matchGroups m)
    span' from to = let b = byteAt from in (b, byteAt to - b)
    byteAt p = IntMap.findWithDefault 0 p table
    table = IntMap.fromDistinctAscList (walk 0 0 t (IntSet.toAscList ends))
    ends  = IntSet.fromList (concatMap endsOf ms)
    endsOf m = matchStart m : matchEnd m
             : concat [[spanStart s, spanEnd s] | Just s <- matchGroups m]
    -- c characters are c' bytes, and rest is what is after them
    -- c 文字が c' バイト。rest は、そのうしろ
    walk _ _ _ [] = []
    walk c c' rest (p : ps) =
      let (skipped, rest') = T.splitAt (p - c) rest
          p' = c' + lengthWord8 skipped
      in  (p, p') : walk p p' rest' ps
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
