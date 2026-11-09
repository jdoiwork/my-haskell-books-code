-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NoFieldSelectors #-}
-- | The routes of the web pages, as a servant record. They answer HTML,
--   and take what a browser's form sends.
-- Web の画面の道。servant のレコードで書く。答えは HTML で、
-- 受け取るのは、ブラウザーのフォームが送るもの
module Tsundoku.Html.Routes
  ( HTML
  , Fields (..)
  , field
  , PageRoutes (..)
  , PartRoutes (..)
  , PageApi
  , pageApi
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Lucid (Html, renderBS)
import Network.HTTP.Media ((//), (/:))
import Servant
import Web.FormUrlEncoded (FromForm (..), toListStable)
-- ANCHOR_END: header

-- ANCHOR: html
-- 📄 A content type for HTML made with lucid: servant needs to know its
--    name (Accept) and how to turn the value into bytes (MimeRender).
-- lucid で作った HTML の、コンテンツの型。servant が知りたいのは、
-- その名前（Accept）と、値をバイト列にするやり方（MimeRender）
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML (Html ()) where
  mimeRender _ = renderBS
-- ANCHOR_END: html

-- ANCHOR: fields
-- 📮 What a form sent: names and values, all text. When a name comes
--    twice, the last one wins (the hidden "" before a checkbox).
-- フォームが送ったもの：名前と値。全部、文字。同じ名前が2回来たら、
-- あとのほうが勝つ（チェックの箱の前に隠した "" のため）
newtype Fields = Fields (Map Text Text)
  deriving stock (Show, Eq)

instance FromForm Fields where
  fromForm form = Right (Fields (Map.fromList (toListStable form)))

-- 🔎 One field, or "" when the form did not send it.
-- フィールド1つ。送られていなければ ""
field :: Text -> Fields -> Text
field name (Fields m) = Map.findWithDefault "" name m
-- ANCHOR_END: fields

-- ANCHOR: routes
-- 🧭 The pages, and the parts of one work that htmx asks for.
-- ページと、htmx が頼む、作品1つの部品
data PageRoutes mode = PageRoutes
  { home   :: mode :- QueryParam "tag" Text :> Get '[HTML] (Html ())
  , card   :: mode :- "works" :> Capture "id" Int :> Get '[HTML] (Html ())
  , part   :: mode :- "works" :> Capture "id" Int :> NamedRoutes PartRoutes
  , search :: mode :- "search" :> QueryParam' '[Required, Strict] "q" Text
                   :> Get '[HTML] (Html ())
  , stats  :: mode :- "stats" :> Get '[HTML] (Html ())
  , static :: mode :- "static" :> Raw
  }
  deriving stock Generic

data PartRoutes mode = PartRoutes
  { rating  :: mode :- "rating"  :> FormBody :> Put  '[HTML] (Html ())
  , reading :: mode :- "reading" :> FormBody :> Post '[HTML] (Html ())
  , notes   :: mode :- "notes"   :> FormBody :> Post '[HTML] (Html ())
  , attrs   :: mode :- "attrs"   :> FormBody :> Post '[HTML] (Html ())
  , flow    :: mode :- "flow"    :> FormBody :> Post '[HTML] (Html ())
  , tags    :: mode :- "tags"    :> FormBody :> Post '[HTML] (Html ())
  }
  deriving stock Generic

-- 📮 Every part takes the fields of a form.
-- どの部品も、フォームのフィールドを受け取る
type FormBody = ReqBody '[FormUrlEncoded] Fields

type PageApi = NamedRoutes PageRoutes

pageApi :: Proxy PageApi
pageApi = Proxy
-- ANCHOR_END: routes
-- ANCHOR_END: all
