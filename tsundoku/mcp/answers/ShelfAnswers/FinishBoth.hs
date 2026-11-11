-- ANCHOR: all
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- | Training: "読み終えた" swaps the state and the bar at once. The
--   answer is the reading part, and the bar out of band (hx-swap-oob).
-- トレーニング：「読み終えた」で、状態とバーを一度に差し替える。答えは
-- 読書の状態の部品と、帯の外（hx-swap-oob）のバー
module ShelfAnswers.FinishBoth (finishApp, finishedParts) where

import Control.Monad.IO.Class (liftIO)
import Data.Time (utctDay)
import Lucid
import Lucid.Base (makeAttribute)
import Servant

import Tsundoku.Api.Json (CardDetail (..), WorkSummary (..), detailOf)
import Tsundoku.Api.Server (Clock)
import Tsundoku.Card (Card (..))
import Tsundoku.Html.Parts (progressBar, readingPart)
import Tsundoku.Html.Routes (HTML)
import Tsundoku.Html.Server (answer, htmxOnly, refuse)
import Tsundoku.Shelf
import Tsundoku.Types

type FinishApi =
  "works" :> Capture "id" Int :> "finish" :> Post '[HTML] (Html ())

-- 🏁 Finish a work: the event Finish, then every entry is read today.
--    Only from htmx, as every change of the pages.
-- 作品を読み終える：できごと Finish のあと、どの項目も今日読んだことに。
-- ページのほかの変更と同じく、htmx からだけ
finishApp :: Clock -> Shelf -> Application
finishApp clock shelf = htmxOnly (serve (Proxy :: Proxy FinishApi) finish)
  where
    finish n = do
      let wid = WorkId n
      today <- utctDay <$> liftIO clock
      found <- liftIO (shelf.works.getCard wid)
      card  <- maybe (refuse (NoSuchWork wid)) pure found
      -- the event first: if it cannot happen now (still piled), say so
      --   before anything is written in the log
      -- できごとが先。いまできないなら（積読のまま）、ログを書く前に断る
      _     <- answer (shelf.reading.applyEvent wid Finish)
      mapM_ (answer . shelf.reading.logReading wid . LogLine today)
            (entryIds card.work.contents)
      after <- liftIO (shelf.works.getCard wid)
      maybe (refuse (NoSuchWork wid)) (pure . finishedParts . (,) wid) after

-- 🧩 The reading part, and the bar that replaces id="bar" wherever it is.
-- 読書の状態の部品と、どこにあっても id="bar" を差し替えるバー
finishedParts :: (WorkId, Card) -> Html ()
finishedParts (wid, card) = do
  readingPart d.work.id d.work.reading d.next
  div_ [id_ "bar", makeAttribute "hx-swap-oob" "true"]
       (maybe "" progressBar d.work.progress)
  where
    d = detailOf (wid, card)
-- ANCHOR_END: all
