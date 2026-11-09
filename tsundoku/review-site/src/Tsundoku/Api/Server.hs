-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE OverloadedRecordDot #-}
-- | The server: every route calls the window, and turns its answer into
--   JSON. Nothing more. It does not know where the cards are kept, nor
--   what the screens look like.
-- サーバー：どの道も窓口を呼んで、その答えを JSON にするだけ。
-- カードがどこにしまわれているかも、画面がどんな見た目かも、知らない
module Tsundoku.Api.Server
  ( Clock
  , shelfApp
  , shelfServer
  , verdict
  ) where

-- IO in a handler, JSON, and the header of the answer
-- ハンドラーの中の IO、JSON、答えのヘッダー
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Network.HTTP.Types (hContentType)

-- servant: the server side, from records of routes
-- servant：サーバーの側を、道のレコードから
import Servant
import Servant.Server.Generic (AsServer)

import Tsundoku.Api.Json
import Tsundoku.Api.Routes
import Tsundoku.Form (explain, formFor)
import Tsundoku.Rating (score)
import Tsundoku.Shelf
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: app
-- ⏰ Where the time of a new note comes from. Tests pass a fixed one.
-- 新しい感想の時刻をどこから取るか。テストでは止まった時計を渡す
type Clock = IO UTCTime

-- 🌐 The whole API as a WAI application, on any shelf. The context
--    carries how servant itself says no (see `servantProblems`).
-- API の全体を、WAI のアプリに。本棚はどれでもよい。コンテキストは、
-- servant 自身が断るときの言い方を運ぶ（servantProblems を見る）
shelfApp :: Clock -> Shelf -> Application
shelfApp clock shelf =
  serveWithContext shelfApi (servantProblems :. EmptyContext)
                   (shelfServer clock shelf)
-- ANCHOR_END: app

-- ANCHOR: shelfServer
-- 🧭 The handlers of the whole shelf. Each one asks the window.
-- 本棚の全体のハンドラー。どれも窓口に聞くだけ
shelfServer :: Clock -> Shelf -> ShelfRoutes AsServer
shelfServer clock shelf = ShelfRoutes
  { listWorks = map summaryOf <$> liftIO shelf.works.listCards
  , addWork   = addOne
  , work      = workServer clock shelf . WorkId
  , search    = \typed -> map hitItem <$> answer (shelf.inside.searchText typed)
  }
  where
    addOne new = do
      what <- known "kind" new.kind (workFrom new)
      wid  <- answer (addChecked shelf.works what)
      cardOf shelf wid
-- ANCHOR_END: shelfServer

-- ANCHOR: workServer
-- 📖 The handlers of one work. The table reads like the window:
--    one route, one field of the window.
-- 作品1つのハンドラー。窓口と同じように読める：道1つに、窓口の1つ
workServer :: Clock -> Shelf -> WorkId -> WorkRoutes AsServer
workServer clock shelf wid = WorkRoutes
  { card       =              cardOf shelf wid
  , remove     =              removeIt
  , happen     = \h        -> happenTo h
  , addNote    = \new      -> writeNote new
  , logReading = \line     -> readOn line
  , rate       = \p        -> rateWith (shelf.reading.rate wid) p
  , rateEntry  = \e p      -> rateWith (shelf.reading.rateEntry wid (EntryId e)) p
  , setText    = \e body   -> done (shelf.inside.setText wid (EntryId e) body)
  , form       =              formOf
  , editAttrs  = \fields   -> answer (shelf.custom.editAttrs wid fields) >> formOf
  }
  where
    removeIt = do
      gone <- liftIO (shelf.works.removeWork wid)
      if gone then pure NoContent else refuse (NoSuchWork wid)
    happenTo h = do
      event <- known "event" h.event (fromName h.event)
      now   <- answer (shelf.reading.applyEvent wid event)
      pure (readingNow now)
    writeNote new = do
      now <- liftIO clock
      let note = noteFrom now new
      answer (shelf.reading.addNote wid note)
      pure (noteItem note)
    readOn line = do
      logLine <- known "day" line.day (logFrom line)
      done (shelf.reading.logReading wid logLine)
    formOf = do
      found  <- liftIO (shelf.works.getCard wid)
      schema <- liftIO shelf.custom.getSchema
      case found of
        Just c  -> pure (map formField (formFor schema c))
        Nothing -> refuse (NoSuchWork wid)
    rateWith keep p = do
      points <- gate (score p.points)
      done (keep points)
-- ANCHOR_END: workServer

-- ANCHOR: helpers
-- 🗂️ The card of a work, or 404.
-- 作品のカード。なければ 404
cardOf :: Shelf -> WorkId -> Handler CardDetail
cardOf shelf wid = do
  found <- liftIO (shelf.works.getCard wid)
  case found of
    Just card -> pure (detailOf (wid, card))
    Nothing   -> refuse (NoSuchWork wid)

-- 🚪 What the core said: go on with the value, or refuse.
-- 核が言ったこと：値で先へ進むか、断るか
gate :: Checked a -> Handler a
gate checked = case checked of
  Right value  -> pure value
  Left mistake -> refuse mistake

-- 🪟 Ask the window, and pass its answer through the gate.
-- 窓口に聞いて、その答えを門に通す
answer :: IO (Checked a) -> Handler a
answer ask = liftIO ask >>= gate

-- ✅ The same, for a change with nothing to show: 204 No Content.
-- 同じことを、見せるもののない変更で：204 No Content
done :: IO (Checked ()) -> Handler NoContent
done ask = do
  answer ask
  pure NoContent

-- 🏷️ A word that came in, read into one of ours, or 400 Bad Request.
-- 入ってきた言葉を、こちらの値に読む。読めなければ 400 Bad Request
known :: Text -> Text -> Maybe a -> Handler a
known what word found = case found of
  Just value -> pure value
  Nothing    -> throwProblem err400 Problem
    { mistake  = "unknown-word"
    , messages = ["「" <> word <> "」は " <> what <> " として読めません"]
    }
-- ANCHOR_END: helpers

-- ANCHOR: verdict
-- ⚖️ Which HTTP status each mistake is, and the word for programs.
--    404: there is no such thing. 409: not now, in this state.
--    422: what came in is not acceptable.
-- まちがいごとの HTTP の状態と、プログラム向けの言葉。
-- 404：そんなものはない。409：いまの状態では、できない。
-- 422：入ってきたものが、受け取れない
verdict :: Mistake -> (ServerError, Text)
verdict mistake = case mistake of
  BadScore      _     -> (err422, "bad-score")
  BadRating     _     -> (err422, "bad-rating")
  NotAllowed    _ _   -> (err409, "not-allowed")
  NoSuchEntry   _     -> (err404, "no-such-entry")
  EmptyNote           -> (err422, "empty-note")
  NoSuchWork    _     -> (err404, "no-such-work")
  BadDefinition _     -> (err422, "bad-definition")
  BadAttrs      _     -> (err422, "bad-attrs")
  NoSuchFlow    _     -> (err404, "no-such-flow")
  NoSuchArrow   _ _ _ -> (err409, "no-such-arrow")
  CannotParse   _     -> (err422, "cannot-parse")
  NotASerial          -> (err409, "not-a-serial")
  EmptySearch         -> (err422, "empty-search")
  EmptyTitle          -> (err422, "empty-title")
  LongTitle     _     -> (err422, "long-title")
  NegativeSize  _     -> (err422, "negative-size")
  BadTag        _     -> (err422, "bad-tag")
  TooManyTags   _     -> (err422, "too-many-tags")

-- 🚫 Refuse with the status of the mistake, and why, as JSON.
-- まちがいの状態で断る。理由を JSON で添えて
refuse :: Mistake -> Handler a
refuse mistake = throwProblem status Problem
  { mistake  = word
  , messages = explain mistake
  }
  where
    (status, word) = verdict mistake

-- 📮 Stop the handler with an error whose body is a Problem.
-- 本体が Problem のエラーで、ハンドラーを止める
throwProblem :: ServerError -> Problem -> Handler a
throwProblem status problem = throwError (problemError status problem)

-- 📦 An error of this status, with a Problem as JSON for its body.
-- この状態のエラー。本体は JSON にした Problem
problemError :: ServerError -> Problem -> ServerError
problemError status problem = status
  { errBody    = encode problem
  , errHeaders = [(hContentType, "application/json;charset=utf-8")]
  }
-- ANCHOR_END: verdict

-- ANCHOR: servantProblems
-- 📮 servant says no by itself, before any handler, when the JSON or
--    the URL cannot be read, or there is no such route. Say it in the
--    same shape too, so a screen reads every refusal the same way.
-- JSON や URL が読めないとき、そんな道がないときは、ハンドラーより前に
-- servant が自分で断る。それも同じ形で言う。画面が、どの断りも同じ
-- やり方で読めるように
servantProblems :: ErrorFormatters
servantProblems = defaultErrorFormatters
  { bodyParserErrorFormatter  = \_ _ why -> said err400 "bad-body"   [why]
  , urlParseErrorFormatter    = \_ _ why -> said err400 "bad-url"    [why]
  , headerParseErrorFormatter = \_ _ why -> said err400 "bad-header" [why]
  , notFoundErrorFormatter    = \_       -> said err404 "no-such-route" []
  }
  where
    said status word whys =
      problemError status (Problem word (map T.pack whys))
-- ANCHOR_END: servantProblems
-- ANCHOR_END: all
