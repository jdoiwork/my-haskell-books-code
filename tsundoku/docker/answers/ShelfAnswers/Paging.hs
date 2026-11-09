-- ANCHOR: all
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- | Training, step 3: the list of works, a page at a time. The gate is
--   in FromHttpApiData: a limit that is not 1 to 100 never reaches the
--   handler (servant answers 400 by itself).
-- トレーニングのステップ3：作品の一覧を、1ページずつ。門番は
-- FromHttpApiData の中：1〜100 でない limit は、ハンドラーまで届かない
-- （servant が自分で 400 を返す）
module ShelfAnswers.Paging
  ( Limit
  , Offset
  , limit
  , offset
  , page
  , PagedRoutes (..)
  , pagedApp
  , pagedClient
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Servant
import Servant.Client (AsClientT, ClientM, client)
import Servant.Server.Generic (AsServer)

import Tsundoku.Api.Json (WorkSummary, summaryOf)
import Tsundoku.Shelf

-- 📏 How many on a page (1 to 100), and how many to skip (0 or more).
--    Only limit and offset make them.
-- 1ページに何件か（1〜100）と、何件とばすか（0 以上）。
-- 作れるのは limit と offset だけ
newtype Limit  = Limit Int  deriving (Show, Eq)
newtype Offset = Offset Int deriving (Show, Eq)

limit :: Int -> Either T.Text Limit
limit n
  | n >= 1 && n <= 100 = Right (Limit n)
  | otherwise          = Left "limit: 1 から 100 まで"

offset :: Int -> Either T.Text Offset
offset n
  | n >= 0    = Right (Offset n)
  | otherwise = Left "offset: 0 以上"

-- 🚪 Read from the query string through the smart constructors.
-- クエリ文字列から、スマートコンストラクターを通して読む
instance FromHttpApiData Limit where
  parseQueryParam t = parseQueryParam t >>= limit

instance FromHttpApiData Offset where
  parseQueryParam t = parseQueryParam t >>= offset

-- 📤 And written back, for the client.
-- クライアントのために、書き戻す
instance ToHttpApiData Limit where
  toQueryParam (Limit n) = toQueryParam n

instance ToHttpApiData Offset where
  toQueryParam (Offset n) = toQueryParam n

-- 📄 One page of a list.
-- リストの1ページ
page :: Limit -> Offset -> [a] -> [a]
page (Limit l) (Offset o) xs = take l (drop o xs)

-- 🧭 GET /works?limit=10&offset=20
data PagedRoutes mode = PagedRoutes
  { pageOfWorks :: mode :- "works"
                         :> QueryParam "limit"  Limit
                         :> QueryParam "offset" Offset
                         :> Get '[JSON] [WorkSummary]
  }
  deriving stock Generic

-- 🪟 No limit: 20. No offset: from the start.
-- limit がなければ 20 件。offset がなければ、はじめから
pagedServer :: Shelf -> PagedRoutes AsServer
pagedServer shelf = PagedRoutes
  { pageOfWorks = \l o -> do
      cards <- liftIO shelf.works.listCards
      let howMany = fromMaybe (Limit 20) l
          skipped = fromMaybe (Offset 0) o
      pure (map summaryOf (page howMany skipped cards))
  }

pagedApp :: Shelf -> Application
pagedApp shelf = serve (Proxy @(NamedRoutes PagedRoutes)) (pagedServer shelf)

pagedClient :: PagedRoutes (AsClientT ClientM)
pagedClient = client (Proxy @(NamedRoutes PagedRoutes))
-- ANCHOR_END: all
