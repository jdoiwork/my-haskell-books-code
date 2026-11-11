-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE OverloadedRecordDot #-}
-- | The web pages: another way in, next to the JSON API. Every handler
--   asks the window, then draws a part with lucid. The JSON API does
--   not change: it still knows no screens.
-- Web の画面：JSON の API のとなりに置く、もう1つの入り口。どのハンドラーも
-- 窓口に聞いて、lucid で部品を描く。JSON の API は変わらない（画面を
-- 知らないまま）
module Tsundoku.Html.Server
  ( pagesApp
  , siteApp
  , htmxOnly
  , localOnly
  , localNames
  , ratingFrom
  , answer
  , refuse
  ) where

-- IO in a handler, text, and reading numbers
-- ハンドラーの中の IO、文字列、数を読む
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

-- HTML, HTTP, WAI, and servant
-- HTML、HTTP、WAI、servant
import Lucid (Html, p_, renderBS)
import Network.HTTP.Types (hContentType, hHost, methodGet, methodHead,
                           status403)
import Network.Wai (Middleware, pathInfo, requestHeaders, requestMethod,
                    responseLBS)
import Servant
import Servant.Server.Generic (AsServer)

import Tsundoku.Api.Json
import Tsundoku.Api.Server (Clock, shelfApp, verdict)
import Tsundoku.Card (Card)
import Tsundoku.Form (explain, formFor)
import Tsundoku.Html.Parts
import Tsundoku.Html.Routes
import Tsundoku.Html.Static (staticFiles)
import Tsundoku.Rating (RatingStyle, Score, parse, score)
import Tsundoku.Shelf
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: site
-- 🌐 The whole site: the JSON API under /api, the pages everywhere else.
--    Both are the same window; only the way in differs.
-- サイトの全体：/api の下は JSON の API、ほかは全部ページ。
-- どちらも同じ窓口で、ちがうのは入り口だけ
siteApp :: RatingStyle -> Clock -> Shelf -> Application
siteApp style clock shelf request reply =
  case pathInfo request of
    "api" : rest -> shelfApp clock shelf request { pathInfo = rest } reply
    _            -> pagesApp style clock shelf request reply

-- 📄 The pages alone. Changes must come from htmx (see htmxOnly).
-- ページだけ。変更は htmx から来たものだけ（htmxOnly を見る）
pagesApp :: RatingStyle -> Clock -> Shelf -> Application
pagesApp style clock shelf =
  htmxOnly (serve pageApi (pageServer style clock shelf))
-- ANCHOR_END: site

-- ANCHOR: htmxOnly
-- 🛡️ A change (anything but GET and HEAD) must carry the header
--    HX-Request, which htmx sends with every request. A form on another
--    site can post to us, but cannot add a header of its own.
-- 変更（GET・HEAD 以外）には、ヘッダー HX-Request が要る。htmx は、どの
-- 要求にもこれを付ける。よそのサイトのフォームは、ここへ送ることは
-- できても、自分のヘッダーを足すことはできない
htmxOnly :: Middleware
htmxOnly app request reply
  | safe || fromHtmx = app request reply
  | otherwise        = reply (responseLBS status403 plain refused)
  where
    safe     = requestMethod request `elem` [methodGet, methodHead]
    fromHtmx = lookup "HX-Request" (requestHeaders request) == Just "true"
    plain    = [(hContentType, "text/plain; charset=utf-8")]
    refused  = "changes come only from the shelf's own pages\n"
-- ANCHOR_END: htmxOnly

-- ANCHOR: localOnly
-- 🏠 The names this machine goes by.
-- このマシンの呼び名
localNames :: [ByteString]
localNames = ["localhost", "127.0.0.1"]

-- 🏠 Only requests addressed to one of these names: the Host header,
--    without its port. A page elsewhere can point a name of its own at
--    127.0.0.1 (DNS rebinding), and then it is "the same site" to the
--    browser. But the Host the browser sends is still that other name.
-- この名前あての要求だけを通す（Host ヘッダーの、ポートを除いた所）。
-- よそのページは、自分の名前を 127.0.0.1 に向けられる（DNS リバイン
-- ディング）。ブラウザーには「同じサイト」に見えるが、ブラウザーが
-- 送る Host は、よその名前のまま
localOnly :: [ByteString] -> Middleware
localOnly names app request reply =
  case lookup hHost (requestHeaders request) of
    Just host | B.takeWhile (/= ':') host `notElem` names ->
      reply (responseLBS status403 plain "only for this machine\n")
    _ -> app request reply
  where
    plain = [(hContentType, "text/plain; charset=utf-8")]
-- ANCHOR_END: localOnly

-- ANCHOR: pageServer
-- 🧭 The pages. Each one asks the window and draws.
-- ページ。どれも窓口に聞いて、描くだけ
pageServer :: RatingStyle -> Clock -> Shelf -> PageRoutes AsServer
pageServer style clock shelf = PageRoutes
  { home   = shelfPage style . map summaryOf <$> liftIO shelf.listCards
  , card   = \wid    -> wholeCard (WorkId wid)
  , part   = \wid    -> partServer style clock shelf (WorkId wid)
  , search = \typed  -> searched typed
  , static = serveDirectoryEmbedded staticFiles
  }
  where
    wholeCard wid = do
      found  <- liftIO (shelf.getCard wid)
      schema <- liftIO shelf.getSchema
      case found of
        Just card -> pure (cardPage style (formFor schema card)
                                          (detailOf (wid, card)))
        Nothing   -> throwError err404
          { errBody    = renderBS (page "本棚にない作品"
                                        (p_ "本棚にない作品です"))
          , errHeaders = [(hContentType, "text/html; charset=utf-8")]
          }
    searched typed = do
      hits <- answer (shelf.searchText typed)
      fine (hitsPart (map hitItem hits))
-- ANCHOR_END: pageServer

-- ANCHOR: partServer
-- 🧩 The parts of one work. Each row: change through the window, then
--    draw the part again from the card as it is now.
-- 作品1つの部品。どの行も：窓口を通して変えて、いまのカードから部品を
-- 描き直す
partServer :: RatingStyle -> Clock -> Shelf -> WorkId -> PartRoutes AsServer
partServer style clock shelf wid = PartRoutes
  { rating  = \fs -> rateFrom  fs >> drawn ratingOf
  , reading = \fs -> happen    fs >> drawn readingOf
  , notes   = \fs -> writeNote fs
  , attrs   = \fs -> editAttrs fs >> drawn attrsOf
  , flow    = \fs -> moveFlow  fs >> drawn attrsOf
  }
  where
    drawn how = do
      (card, schema) <- cardAndSchema shelf wid
      fine (how card schema)
    ratingOf  card _      = ratingPart style n (summary card).rating
    readingOf card _      = readingPart n (summary card).reading
                                          (detailOf (wid, card)).next
    attrsOf   card schema = attrsPart n (formFor schema card)
    summary   card        = summaryOf (wid, card)
    WorkId n = wid

    rateFrom fs = case ratingFrom style fs of
      Nothing      -> pure ()                   -- untouched / さわっていない
      Just checked -> do
        points <- gate checked
        answer (shelf.rate wid points)
    happen fs = do
      event <- known "event" (field "event" fs) (fromName (field "event" fs))
      _     <- answer (shelf.applyEvent wid event)
      pure ()
    writeNote fs = do
      new  <- known "entry" (field "entry" fs) (newNote fs)
      now  <- liftIO clock
      let note = noteFrom now new
      answer (shelf.addNote wid note)
      (card, _) <- cardAndSchema shelf wid
      fine (notePart (detailOf (wid, card)).toc (noteItem note))
    editAttrs (Fields m) = answer (shelf.editAttrs wid m)
    moveFlow fs = answer (shelf.moveFlow wid (field "flow" fs)
                                             (field "to"   fs))
-- ANCHOR_END: partServer

-- ANCHOR: ratingFrom
-- ⭐ What the rating form asks for. A mark sends its points. A typed
--    rating is read back with the style, but only when it was changed:
--    the shadow (★3.5) never rebuilds the model (73 points).
--    Nothing: there is nothing to change.
-- 評価のフォームが頼んでいること。目盛りのボタンは、その点を送る。
-- 打ちこんだ評価は、見せ方で読み戻す。ただし、変えたときだけ：
-- 影（★3.5）から、モデル（73点）を作り直さない。
-- Nothing なら、変えるものはない
ratingFrom :: RatingStyle -> Fields -> Maybe (Checked Score)
ratingFrom style fs
  | points /= ""             = Just (readPoints points)
  | typed  == field "shown" fs = Nothing
  | otherwise                = Just (parse style typed)
  where
    points = field "points" fs
    typed  = field "typed"  fs
    readPoints t = maybe (Left (BadRating t)) score (digitsOnly 3 t)

-- 📝 A new note from the form. Nothing if the entry is not a number.
-- フォームから新しい感想を。項目が数でなければ Nothing
newNote :: Fields -> Maybe NewNote
newNote fs = do
  entry <- case field "entry" fs of
    ""   -> Just Nothing
    word -> Just <$> digitsOnly 9 word
  pure NewNote
    { entry   = entry
    , body    = field "body" fs
    , spoiler = field "spoiler" fs /= ""
    }

-- 🔢 A number in digits only, at most this many of them. readMaybe
--    alone takes "0x50" as 80 and " 3 " as 3, and wraps a number too
--    big for Int around to another one.
-- 数字だけで、このけた数までの数。readMaybe だけだと "0x50" を 80、
-- " 3 " を 3 と読み、Int に入らない数を、輪を回して別の数にする
digitsOnly :: Int -> Text -> Maybe Int
digitsOnly most t
  | not (T.null t), T.all isDigit t, T.length t <= most
              = readMaybe (T.unpack t)
  | otherwise = Nothing
-- ANCHOR_END: ratingFrom

-- ANCHOR: helpers
-- 🗂️ The card of a work and the definitions, or a 404 page.
-- 作品のカードと定義。なければ 404 のページ
cardAndSchema :: Shelf -> WorkId -> Handler (Card, Schema)
cardAndSchema shelf wid = do
  found  <- liftIO (shelf.getCard wid)
  schema <- liftIO shelf.getSchema
  case found of
    Just card -> pure (card, schema)
    Nothing   -> refuse (NoSuchWork wid)

-- ✅ A part that went well. The problem box is sent along, empty, out
--    of band, so an old message goes away.
-- うまくいった部品。problem の箱を、空にして帯の外で添える。
-- 前のまずかったことの言葉が消えるように
fine :: Html () -> Handler (Html ())
fine part = pure (part <> problemBox [])

-- 🚪 What the core said: go on, or refuse.
-- 核が言ったこと：先へ進むか、断るか
gate :: Checked a -> Handler a
gate checked = either refuse pure checked

-- 🪟 Ask the window, and pass its answer through the gate.
-- 窓口に聞いて、その答えを門に通す
answer :: IO (Checked a) -> Handler a
answer ask = liftIO ask >>= gate

-- 🚫 Refuse with the status the API would use, and the problem box.
-- API と同じ状態で断る。本体は problem の箱
refuse :: Mistake -> Handler a
refuse mistake = refuseWith (fst (verdict mistake)) (explain mistake)

-- 🏷️ A word that came in, read into one of ours, or 400.
-- 入ってきた言葉を、こちらの値に読む。読めなければ 400
known :: Text -> Text -> Maybe a -> Handler a
known what word found = case found of
  Just value -> pure value
  Nothing    -> refuseWith err400
                  ["「" <> word <> "」は " <> what <> " として読めません"]

-- 📮 Stop with this status; the body is the problem box, as HTML.
-- この状態で止める。本体は HTML の problem の箱
refuseWith :: ServerError -> [Text] -> Handler a
refuseWith status messages = throwError status
  { errBody    = renderBS (problemBox messages)
  , errHeaders = [(hContentType, "text/html; charset=utf-8")]
  }
-- ANCHOR_END: helpers
-- ANCHOR_END: all
