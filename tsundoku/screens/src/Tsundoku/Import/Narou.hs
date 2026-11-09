-- ANCHOR: all
-- ANCHOR: header
-- | Check web novels on 小説家になろう for new episodes, with its
--   official API (なろう小説API, https://dev.syosetu.com/man/api/). We
--   only use what the API gives (how many episodes, finished or not,
--   when the last one came out). We never read the pages of the site
--   by machine: its rules do not allow it. For personal, non-commercial
--   use only.
-- 小説家になろうの Web 小説に新しい話が出たかを、公式の API（なろう小説
-- API）で確かめる。使うのは API が出す情報だけ（話数、完結したか、最後の
-- 話がいつ出たか）。サイトのページを機械で読むことはしない（規約が認めて
-- いない）。自分用・非商用でだけ使う
module Tsundoku.Import.Narou
  ( Novel (..)
  , Run (..)
  , Answer (..)
  , narou
  , novelsUrl
  , parseNovels
  , fetchNovels
  ) where

-- exceptions, bytes, text, time, and gzip
-- 例外、バイト列、文字列、日時、gzip
import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Zlib.Internal (DecompressError)
import Control.Exception (evaluate, try)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (LocalTime, defaultTimeLocale, parseTimeM)

-- JSON (aeson) and HTTP (http-client)
-- JSON（aeson）と HTTP（http-client）
import Data.Aeson
  (FromJSON (..), eitherDecode, withArray, withObject, (.:))
import Network.HTTP.Client (Manager)

import Tsundoku.Import.Http (getUrl)
import Tsundoku.Import.Types
-- ANCHOR_END: header

-- ANCHOR: novel
-- 📱 What the API tells us about a novel.
-- API が教えてくれる、作品のこと
data Novel = Novel
  { ncode      :: Text       -- "N0000XX" (the API writes it in capitals)
  , novelTitle :: Text
  , writer     :: Text
  , episodes   :: Int        -- general_all_no: episodes so far / いまの話数
  , run        :: Run
  , lastPosted :: LocalTime  -- general_lastup, in Japan time / 日本時間
  }
  deriving (Show, Eq)

-- 🏃 Still coming out, finished, or a short story (one episode).
-- 連載中、完結、短編（1話だけ）
data Run = Ongoing | Completed | ShortStory
  deriving (Show, Eq)

-- 📨 The answer is a list whose first item is different: how many
--    novels matched, and then the novels.
--    [{"allcount": 1}, {"title": ..., "ncode": ...}]
-- 答えはリストで、最初の1つだけ形がちがう：当てはまった作品の数。
-- そのあとに作品が並ぶ
data Answer = Answer
  { allCount :: Int
  , novels   :: [Novel]
  }
  deriving (Show, Eq)

instance FromJSON Answer where
  parseJSON = withArray "なろう小説API の答え" $ \items ->
    case toList items of
      first : rest -> Answer <$> withObject "allcount" (.: "allcount") first
                             <*> traverse parseJSON rest
      []           -> fail "答えが空っぽ"

instance FromJSON Novel where
  parseJSON = withObject "作品" $ \o -> do
    posted <- o .: "general_lastup"
    Novel <$> o .: "ncode"
          <*> o .: "title"
          <*> o .: "writer"
          <*> o .: "general_all_no"
          <*> (runOf <$> o .: "noveltype" <*> o .: "end")
          <*> parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S" posted
    where
      -- noveltype: 1 serial, 2 short. end: 1 still coming out, 0 not.
      -- noveltype は 1 が連載、2 が短編。end は 1 が連載中、0 がそうでない
      runOf :: Int -> Int -> Run
      runOf 2 _ = ShortStory
      runOf _ 1 = Ongoing
      runOf _ _ = Completed
-- ANCHOR_END: novel

-- ANCHOR: url
-- 🌐 Where the API answers, and what to ask: JSON, gzipped, only the
--    fields we use (`of`), every novel at once (ncode joined by -).
-- API の窓口と、聞き方：JSON で、gzip で縮めて、使う項目だけ（of）、
-- 作品をまとめて1回で（ncode を - でつなぐ）
narou :: String
narou = "https://api.syosetu.com/novelapi/api/"

novelsUrl :: String -> [Text] -> String
novelsUrl base codes = base <> "?" <> T.unpack (T.intercalate "&" params)
  where
    params =
      [ "out=json"
      , "gzip=5"
      , "of=n-t-w-ga-nt-e-gl"
      , "lim=" <> T.pack (show (max 1 (length codes)))
      , "ncode=" <> T.intercalate "-" (map T.toLower codes)
      ]
-- ANCHOR_END: url

-- ANCHOR: parse
-- 📖 Read the answer (already unzipped).
-- 答えを読む（gzip は外してある）
parseNovels :: LBS.ByteString -> Either ImportProblem [Novel]
parseNovels bytes = case eitherDecode bytes of
  Left why    -> Left (Unexpected (T.pack why))
  Right found -> Right (novels found)

-- 🌐 Ask about several novels in one go. The body comes back gzipped
--    (we asked for that with gzip=5), so unzip it first.
-- いくつかの作品を、1回で聞く。答えは gzip で縮めてある（gzip=5 で頼んだ）
-- ので、先に戻す
fetchNovels :: Manager -> String -> [Text]
            -> IO (Either ImportProblem [Novel])
fetchNovels manager base codes = do
  answer <- getUrl manager (novelsUrl base codes)
  case answer of
    Left why  -> pure (Left why)
    Right raw -> do
      body <- gunzip raw
      pure (parseNovels =<< body)

-- 🗜️ Unzip all of it here (toStrict reads it to the end), so that a
--    broken gzip fails inside try, not later in someone else's hands.
-- ここで全部を戻す（toStrict が最後まで読む）。壊れた gzip が、try の中で
-- 失敗するように。あとで、ほかの場所で爆発しないように
gunzip :: LBS.ByteString -> IO (Either ImportProblem LBS.ByteString)
gunzip raw = do
  result <- try (evaluate (LBS.toStrict (GZip.decompress raw)))
  pure $ case result of
    Left err   -> Left (Unexpected (T.pack (show (err :: DecompressError))))
    Right body -> Right (LBS.fromStrict body)
-- ANCHOR_END: parse

-- ANCHOR_END: all
