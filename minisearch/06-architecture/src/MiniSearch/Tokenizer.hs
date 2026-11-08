-- ANCHOR: head
-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | Everything that changes with the language, in one record.
-- 言語によって変わるものを、1つのレコードに集める。
module MiniSearch.Tokenizer
  ( Tokenizer (..)
  , english
  , japanese
  , tokenizers
  , tokenizerByName
  ) where

import Data.List (find)
import Data.Text (Text)

import MiniSearch.Bigram (bigramTokenize, charSnippet)
import MiniSearch.Snippet (snippet)
import MiniSearch.Tokenize (tokenize)
import MiniSearch.Types (Token)
-- ANCHOR_END: head

-- ANCHOR: record
-- 🔧 A tokenizer: a name, how to cut a text into tokens,
--    and how to show a piece of a body for a query.
-- トークナイザー：名前と、文章をトークンに切る方法と、
-- 問い合わせに合わせて本文の抜粋を見せる方法
data Tokenizer = Tokenizer
  { tokenizerName :: Text
  , cut           :: Text -> [Token]
  , excerptOf     :: Text -> Text -> Text  -- query -> body -> excerpt
  }
-- ANCHOR_END: record

-- ANCHOR: tokenizers
-- 🇬🇧 English: words, without stop words.
-- 英語：単語に切り、ストップワードを捨てる
english :: Tokenizer
english = Tokenizer
  { tokenizerName = "en"
  , cut           = tokenize
  , excerptOf     = \query body -> snippet 5 (tokenize query) body
  }

-- 🇯🇵 Japanese: two characters at a time.
-- 日本語：2文字ずつ
japanese :: Tokenizer
japanese = Tokenizer
  { tokenizerName = "ja"
  , cut           = bigramTokenize
  , excerptOf     = charSnippet 15
  }

-- 📇 All the tokenizers, to find one by its name.
-- すべてのトークナイザー（名前で探すため）
tokenizers :: [Tokenizer]
tokenizers = [english, japanese]

tokenizerByName :: Text -> Maybe Tokenizer
tokenizerByName name = find (\t -> tokenizerName t == name) tokenizers
-- ANCHOR_END: tokenizers
-- ANCHOR_END: all
