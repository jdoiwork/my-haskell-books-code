-- ANCHOR: head
-- ANCHOR: all
-- | The server's settings, read from the environment variables.
--   A pure function: easy to test, and a mistake is found at start
--   (the first one: Either stops at the first Left).
-- サーバーの設定を、環境変数から読む。
-- 純粋な関数なので、テストしやすく、まちがいは起動のときに見つかる
-- （Either は最初の Left で止まるので、最初の1つ）。
module MiniSearch.Config
  ( Config (..)
  , loadConfig
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import MiniSearch.Number (wholeNumber)
import MiniSearch.Tokenizer (Tokenizer, tokenizerByName, tokenizerNames)
-- ANCHOR_END: head

-- ANCHOR: config
-- ⚙️ Everything the server needs to start.
-- サーバーが起動するのに要るもの全部
data Config = Config
  { configDocs      :: FilePath
  , configPort      :: Int
  , configTokenizer :: Tokenizer
  }

-- 📋 From the list of environment variables to a Config.
--    MINISEARCH_DOCS (samples/docs), PORT (8080), MINISEARCH_LANG (en)
-- 環境変数のリストから Config へ
loadConfig :: [(String, String)] -> Either String Config
loadConfig env =
  Config <$> pure docs <*> port <*> tokenizer
  where
    setting name def = fromMaybe def (lookup name env)
    docs = setting "MINISEARCH_DOCS" "samples/docs"
    port = case wholeNumber (setting "PORT" "8080") of
      Just n | n >= 1 && n <= 65535 -> Right n
      -- 「PORT は 1 から 65535 まで」
      _ -> Left ("PORT must be 1 to 65535: " <> setting "PORT" "")
    tokenizer =
      let lang = setting "MINISEARCH_LANG" "en"
      in case tokenizerByName (T.pack lang) of
           Just tok -> Right tok
           -- 「MINISEARCH_LANG は en, ja のどれか」
           Nothing  -> Left ("MINISEARCH_LANG must be one of "
                             <> T.unpack tokenizerNames <> ": " <> lang)
-- ANCHOR_END: config
-- ANCHOR_END: all
