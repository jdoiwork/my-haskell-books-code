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
import Data.Aeson
  ( FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..)
  , ToJSON (..), ToJSONKey (..), withText )
import Data.Aeson.Types (toJSONKeyText)
import Data.Binary (Binary)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Generics (Generic)
import Numeric (readHex)
import Text.Printf (printf)

-- ANCHOR: document
-- ANCHOR: docid
-- 🔢 The id of a document: the first 8 bytes (64 bits) of a hash of
--    its file (MiniSearch.Load), as a number. Not just a Word64:
--    a DocId can never be mixed up with a count or a score.
-- 文書の番号：ファイルのハッシュ値の頭の8バイト（64ビット）を、
-- 数にしたもの（MiniSearch.Load）。ただの Word64 ではない：
-- 個数や点数と、取りちがえることがない
newtype DocId = DocId Word64
  deriving stock    (Eq, Ord, Show)
  deriving newtype  (NFData, Binary)
-- ANCHOR_END: docid

-- ANCHOR: docidText
-- 🔡 The id as 16 hex digits, for people and for JSON:
--    DocId 6 -> "0000000000000006".
--    (JSON numbers above 2^53 lose digits in JavaScript.)
-- 人と JSON のために、番号を16進の16文字にする。
-- （JSON の数は、2^53 を超えると JavaScript でけたが落ちる）
docIdText :: DocId -> Text
docIdText (DocId n) = T.pack (printf "%016x" n)

-- 🔡 Back from 16 hex digits. Anything else is not an id.
-- 16進の16文字から戻す。それ以外は、番号ではない
docIdFromText :: Text -> Maybe DocId
docIdFromText t
  | T.length t == 16 && T.all (`elem` hexDigits) t =
      case readHex (T.unpack t) of
        [(n, "")] -> Just (DocId n)
        _         -> Nothing
  | otherwise = Nothing
  where
    hexDigits = "0123456789abcdef" :: String

-- 📦 In JSON, an id is a string of 16 hex digits,
--    as a value and as a key.
-- JSON では、番号は16進の16文字の文字列（値でも、キーでも）
instance ToJSON DocId where
  toJSON = toJSON . docIdText

instance FromJSON DocId where
  parseJSON = withText "DocId" $ \t ->
    maybe (fail ("not a document id: " <> T.unpack t)) pure
          (docIdFromText t)

instance ToJSONKey DocId where
  toJSONKey = toJSONKeyText docIdText

instance FromJSONKey DocId where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    maybe (fail ("not a document id: " <> T.unpack t)) pure
          (docIdFromText t)
-- ANCHOR_END: docidText

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
type Postings = Map DocId Int

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
  , indexDocs     :: !(Map DocId Document)
    -- How many tokens each document has.
    -- 文書ごとの単語の数
  , indexLengths  :: !(Map DocId Int)
  }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData, Binary)

-- ANCHOR: monoid
-- 🧩 Two indexes of DIFFERENT documents can be put together.
--    (The same DocId must not be in both.)
-- 「別々の文書」のインデックス2つは、合わせられる。
-- （同じ DocId が両方にあってはいけない）
instance Semigroup Index where
  a <> b = Index
    { indexPostings =
        Map.unionWith Map.union (indexPostings a) (indexPostings b)
    , indexDocs     = Map.union (indexDocs a) (indexDocs b)
    , indexLengths  = Map.union (indexLengths a) (indexLengths b)
    }

-- 📭 The empty index changes nothing when put together.
-- 空のインデックスは、合わせても何も変えない
instance Monoid Index where
  mempty = Index Map.empty Map.empty Map.empty
-- ANCHOR_END: monoid
-- ANCHOR_END: index

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
