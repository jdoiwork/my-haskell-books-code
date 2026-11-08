-- ANCHOR: head
-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | Japanese: cut the text into pieces of two characters (bigrams).
-- 日本語：文章を2文字ずつ（bigram）に切る。
module MiniSearch.Bigram
  ( bigramTokenize
  , charSnippet
  ) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

import MiniSearch.Types (Token (..))
-- ANCHOR_END: head

-- ANCHOR: kind
-- 🈁 What kind of character: Japanese, a letter or digit, or other.
-- 文字の種類：日本語か、英数字か、それ以外か
data Kind = Japanese | AlphaNum | Other
  deriving (Eq)

kindOf :: Char -> Kind
kindOf c
  | isJapanese c = Japanese
  | isAlphaNum c = AlphaNum
  | otherwise    = Other

-- Hiragana, katakana (and the long sound mark), and kanji.
-- ひらがな、カタカナ（と長音の記号）、漢字
isJapanese :: Char -> Bool
isJapanese c =
     ('\x3041' <= c && c <= '\x309F')
  || ('\x30A0' <= c && c <= '\x30FF')
  || ('\x4E00' <= c && c <= '\x9FFF')
-- ANCHOR_END: kind

-- ANCHOR: tokenize
-- ✂️ "赤いドラゴン" -> [赤い, いド, ドラ, ラゴ, ゴン]
--    Letters and digits stay whole words: "HP 30" -> [hp, 30]
-- 英数字は、単語のまま
bigramTokenize :: Text -> [Token]
bigramTokenize text =
  let runs = T.groupBy (\a b -> kindOf a == kindOf b) (T.toLower text)
  in concatMap tokensOfRun runs
  where
    tokensOfRun run = case kindOf (T.head run) of
      Japanese -> map Token (bigrams run)
      AlphaNum -> [Token run]
      Other    -> []

-- 🧩 "ドラゴン" -> ["ドラ", "ラゴ", "ゴン"]; one character stays as it is.
-- 1文字だけなら、そのまま
bigrams :: Text -> [Text]
bigrams t
  | T.length t <= 1 = [t]
  | otherwise       = [ T.take 2 (T.drop i t) | i <- [0 .. T.length t - 2] ]
-- ANCHOR_END: tokenize

-- ANCHOR: snippet
-- ✂️ Some characters around the first word of the query found in
--    the body, with every query word in [brackets].
-- 本文の中で最初に見つかった問い合わせの語のまわりの文字。
-- 問い合わせの語は、すべて [ ] で囲む
charSnippet :: Int -> Text -> Text -> Text
charSnippet width query body =
  let flat   = T.unwords (T.words body)
      words' = filter (not . T.null) (T.words query)
      places = [ T.length before
               | w <- words'
               , let (before, rest) = T.breakOn w flat
               , not (T.null rest) ]
      first  = if null places then 0 else minimum places
      start  = max 0 (first - width)
      piece  = T.take (2 * width) (T.drop start flat)
      marked = foldr mark piece words'
      before' = if start > 0 then "…" else ""
      after'  = if start + T.length piece < T.length flat then "…" else ""
  in before' <> marked <> after'
  where
    mark w t = T.intercalate ("[" <> w <> "]") (T.splitOn w t)
-- ANCHOR_END: snippet
-- ANCHOR_END: all
