-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | The search API, with "did you mean ...?" (step 5).
-- 「もしかして」の付いた検索 API（ステップ5）。
module Review.SuggestApi
  ( SuggestApi
  , SuggestResponse (..)
  , suggestApp
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant

import MiniSearch.Api (HitView (..))
import MiniSearch.Live
import MiniSearch.Query
import MiniSearch.Search (searchTokens)
import MiniSearch.Tokenizer (Tokenizer (..))
import MiniSearch.Types
import Review.Suggest (Answer (..), answer)

-- ANCHOR: response
-- 📦 The answer to a search, with one more field.
-- 検索への答え。フィールドを1つ足した
data SuggestResponse = SuggestResponse
  { query      :: Text
  , total      :: Int
  , hits       :: [HitView]
  , suggestion :: Maybe Text  -- "dragon fire", or null / もしかして
  }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
-- ANCHOR_END: response

-- 🗺️ GET /search?q=dargon+fier&limit=5
type SuggestApi =
  "search" :> QueryParam "q" Text :> QueryParam "limit" Int
    :> Get '[JSON] SuggestResponse

-- ANCHOR: handler
-- 🏰 Search; only when nothing is found, look for a suggestion.
-- 検索する。何も当たらなかったときだけ、もしかしてを探す
suggestApp :: Live -> Application
suggestApp live = serve (Proxy @SuggestApi) handler
  where
    tok = liveTokenizer live
    handler mq mlimit =
      case (,) <$> parseSearchQuery tok (fromMaybe "" mq)
               <*> parseLimit mlimit of
        Left _ -> throwError err400
        Right (q, lim) -> do
          index <- liftIO (currentIndex live)
          let found = searchTokens index (queryTokens q)
              shown = take (limitValue lim) found
          pure SuggestResponse
            { query      = queryText q
            , total      = length found
            , hits       = map (toView index (queryText q)) shown
            , suggestion = suggestFor index found (queryText q)
            }

    -- answer cuts words the English way, so ask it only in English.
    -- answer は英語の切り方なので、英語の設定のときだけ聞く
    suggestFor index found q
      | null found, tokenizerName tok == "en"
      , DidYouMean fixed <- answer index q = Just fixed
      | otherwise                          = Nothing

    toView index q h =
      let DocId n = hitDoc h
          body'   = maybe "" docBody (Map.lookup (hitDoc h) (indexDocs index))
      in HitView n (hitTitle h) (hitScore h) (excerptOf tok q body')
-- ANCHOR_END: handler
-- ANCHOR_END: all
