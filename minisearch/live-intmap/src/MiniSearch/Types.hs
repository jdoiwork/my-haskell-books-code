-- ANCHOR: head
-- ANCHOR: all
{-# LANGUAGE DeriveAnyClass #-}
-- | The words of the search engine, as types.
-- 検索エンジンの言葉を、型にする。
module MiniSearch.Types
  ( DocId (..)
  , docIdText
  , docIdFromText
  , Document (..)
  , Token (..)
  , Postings
  , Index (..)
  , Hit (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import Data.Binary (Binary)
import Data.Map (Map)
import MiniSearch.Types.DocId
import MiniSearch.DocMap (DocMap)
import qualified MiniSearch.DocMap as DM
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
-- ANCHOR_END: head

-- ANCHOR: document

-- 📄 One document to search.
-- 検索の対象になる、文書ひとつ
data Document = Document
  { docId    :: DocId
  , docTitle :: Text
  , docBody  :: Text
  }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData, Binary)
-- ANCHOR_END: document

-- ANCHOR: token
-- 🔤 A word after cleaning: lower case, no punctuation.
--    "Dragon," and "dragon" become the same Token "dragon".
-- 整えたあとの単語：小文字で、記号なし。
-- "Dragon," も "dragon" も、同じ Token "dragon" になる
newtype Token = Token Text
  deriving stock    (Eq, Ord, Show)
  deriving newtype  (ToJSON, FromJSON, ToJSONKey, FromJSONKey, NFData, Binary)
-- ANCHOR_END: token

-- ANCHOR: index
-- 📮 For one token: which documents have it, and how many times.
-- ある単語について：どの文書に、何回出てくるか
type Postings = DocMap Int

-- 📚 The inverted index: from a token to its postings.
--    Like the index at the back of a book.
-- 転置インデックス：単語から、出てくる場所へ。
-- 本の巻末の索引と同じ
-- The ! makes each field strict: computed when the Index is made.
-- ! で、それぞれのフィールドを正格にする：Index を作るときに計算される
data Index = Index
  { indexPostings :: !(Map Token Postings)
    -- The documents themselves, to show titles.
    -- 文書そのもの（題を見せるため）
  , indexDocs     :: !(DocMap Document)
    -- How many tokens each document has.
    -- 文書ごとの単語の数
  , indexLengths  :: !(DocMap Int)
  }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData, Binary)
-- ANCHOR_END: index

-- ANCHOR: monoid
-- 🧩 Two indexes of DIFFERENT documents can be put together.
--    (The same DocId must not be in both.)
-- 「別々の文書」のインデックス2つは、合わせられる。
-- （同じ DocId が両方にあってはいけない）
instance Semigroup Index where
  a <> b = Index
    { indexPostings =
        Map.unionWith DM.union (indexPostings a) (indexPostings b)
    , indexDocs     = DM.union (indexDocs a) (indexDocs b)
    , indexLengths  = DM.union (indexLengths a) (indexLengths b)
    }

-- 📭 The empty index changes nothing when put together.
-- 空のインデックスは、合わせても何も変えない
instance Monoid Index where
  mempty = Index Map.empty DM.empty DM.empty
-- ANCHOR_END: monoid

-- ANCHOR: hit
-- 🎯 One search result.
-- 検索結果ひとつ
data Hit = Hit
  { hitDoc   :: DocId
  , hitTitle :: Text
  , hitScore :: Double
  }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)
-- ANCHOR_END: hit
-- ANCHOR_END: all
