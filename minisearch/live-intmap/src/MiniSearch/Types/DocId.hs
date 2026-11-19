-- | The id of a document, in its own module: MiniSearch.DocMap needs it,
--   and MiniSearch.Types needs DocMap. (The same code as in live's Types.)
-- 文書の番号だけのモジュール。MiniSearch.DocMap がこれを使い、
-- MiniSearch.Types が DocMap を使うので、輪にならないように分けた
-- （中身は live の段階の Types と同じ）。
module MiniSearch.Types.DocId (DocId (..), docIdText, docIdFromText) where
import Control.DeepSeq (NFData)
import Data.Aeson
  ( FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..)
  , ToJSON (..), ToJSONKey (..), withText )
import Data.Aeson.Types (toJSONKeyText)
import Data.Binary (Binary)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Numeric (readHex)
import Text.Printf (printf)
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
