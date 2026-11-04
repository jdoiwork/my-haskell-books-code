{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Answers to the training of the architecture chapter.
-- 設計を磨く章のトレーニングの解答。
module MiniSearch.Answers
  ( unigramsToo
  , japaneseWithUnigrams
  , dropPlural
  , englishPlural
  ) where

import qualified Data.Text as T

import MiniSearch.Bigram (bigramTokenize, charSnippet)
import MiniSearch.Snippet (snippet)
import MiniSearch.Tokenize (tokenize)
import MiniSearch.Tokenizer (Tokenizer (..))
import MiniSearch.Types (Token (..))

-- ANCHOR: unigrams
-- 🈁 Bigrams, plus every Japanese character on its own,
--    so that a one-character query like "炎" can match.
-- bigram に加えて、日本語の1文字ずつも入れる。
-- 「炎」のような1文字の問い合わせも当たるように
unigramsToo :: T.Text -> [Token]
unigramsToo text =
  let grams = bigramTokenize text
      singles = [ Token (T.singleton c)
                | Token t <- grams, T.length t == 2, c <- T.unpack t ]
  in grams <> singles

japaneseWithUnigrams :: Tokenizer
japaneseWithUnigrams = Tokenizer
  { tokenizerName = "ja1"
  , cut           = unigramsToo
  , excerptOf     = charSnippet 15
  }
-- ANCHOR_END: unigrams

-- ANCHOR: plural
-- 🐉 "dragons" -> "dragon": a very simple plural rule.
-- "dragons" -> "dragon"：とても簡単な複数形の決まり
dropPlural :: Token -> Token
dropPlural (Token w)
  | T.length w > 3, "s" `T.isSuffixOf` w, not ("ss" `T.isSuffixOf` w)
              = Token (T.dropEnd 1 w)
  | otherwise = Token w

englishPlural :: Tokenizer
englishPlural = Tokenizer
  { tokenizerName = "en-plural"
  , cut           = map dropPlural . tokenize
  , excerptOf     = \query body -> snippet 5 (tokenize query) body
  }
-- ANCHOR_END: plural
-- ANCHOR_END: all
