-- ANCHOR: head
-- ANCHOR: all
-- | The words of the search engine, as types.
-- 検索エンジンの言葉を、型にする。
module MiniSearch.Types
  ( DocId (..)
  , Document (..)
  , Token (..)
  , Postings
  , Index (..)
  , Hit (..)
  ) where

import Data.Map (Map)
import Data.Text (Text)
-- ANCHOR_END: head

-- ANCHOR: document
-- 🔢 The number of a document. Not just an Int:
--    a DocId can never be mixed up with a count or a score.
-- 文書の番号。ただの Int ではない：
-- 個数や点数と、取りちがえることがない
newtype DocId = DocId Int
  deriving (Eq, Ord, Show)

-- 📄 One document to search.
-- 検索の対象になる、文書ひとつ
data Document = Document
  { docId    :: DocId
  , docTitle :: Text
  , docBody  :: Text
  }
  deriving (Eq, Show)
-- ANCHOR_END: document

-- ANCHOR: token
-- 🔤 A word after cleaning: lower case, no punctuation.
--    "Dragon," and "dragon" become the same Token "dragon".
-- 整えたあとの単語：小文字で、記号なし。
-- "Dragon," も "dragon" も、同じ Token "dragon" になる
newtype Token = Token Text
  deriving (Eq, Ord, Show)
-- ANCHOR_END: token

-- ANCHOR: index
-- 📮 For one token: which documents have it, and how many times.
-- ある単語について：どの文書に、何回出てくるか
type Postings = Map DocId Int

-- 📚 The inverted index: from a token to its postings.
--    Like the index at the back of a book.
-- 転置インデックス：単語から、出てくる場所へ。
-- 本の巻末の索引と同じ
data Index = Index
  { indexPostings :: Map Token Postings
    -- The documents themselves, to show titles.
    -- 文書そのもの（題を見せるため）
  , indexDocs     :: Map DocId Document
    -- How many tokens each document has.
    -- 文書ごとの単語の数
  , indexLengths  :: Map DocId Int
  }
  deriving (Eq, Show)
-- ANCHOR_END: index

-- ANCHOR: hit
-- 🎯 One search result.
-- 検索結果ひとつ
data Hit = Hit
  { hitDoc   :: DocId
  , hitTitle :: Text
  , hitScore :: Double
  }
  deriving (Eq, Show)
-- ANCHOR_END: hit
-- ANCHOR_END: all
