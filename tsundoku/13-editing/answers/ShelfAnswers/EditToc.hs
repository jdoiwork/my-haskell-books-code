-- ANCHOR: all
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- | Training: fix a work with htmx. The table is one textarea, a line for
--   each entry, indented two spaces a level. A line that starts with "#3"
--   keeps the number 3; a line with no "#" is new. The server reads the
--   lines back (readEdited) and hands the revision to the same window as
--   React does (reviseWork).
-- トレーニング：htmx で作品を直す。目次は textarea 1つで、項目ごとに1行、
-- 1段は空白2つ。"#3" で始まる行は番号 3 を保ち、"#" のない行は新しい行。
-- サーバーが行を読み戻し（readEdited）、React と同じ窓口（reviseWork）に
-- 渡す
module ShelfAnswers.EditToc
  ( tocText
  , readEdited
  , editForm
  , editApp
  ) where

-- text, characters, trees, and the web
-- 文字列、文字、木、Web
import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Forest)
import Lucid
import Servant

import Tsundoku.Api.Json (CardDetail (..), TocItem (..), WorkSummary (..),
                          detailOf)
import Tsundoku.Form (explain)
import Tsundoku.Html.Parts (hxPut, hxSwap, hxTarget, problemBox)
import Tsundoku.Html.Routes (Fields, HTML, field)
import Tsundoku.Outline (fromOutline)
import Tsundoku.Shelf
import Tsundoku.Types

-- ANCHOR: text
-- 📝 The table as lines to edit: "#3 1.1 うろこの色 8".
-- 直すための目次の行："#3 1.1 うろこの色 8"
tocText :: [TocItem] -> Text
tocText = T.unlines . concatMap (line 0)
  where
    line :: Int -> TocItem -> [Text]
    line depth t =
      T.concat [ T.replicate depth "  ", "#", shown t.id, " "
               , t.heading, " ", shown t.size ]
        : concatMap (line (depth + 1)) t.children
    shown = T.pack . show

-- ✍️ Read the lines back. Every line that cannot be read is said, with
--    its line number (as the React screen does).
-- 行を読み戻す。読めない行は全部、何行目かを添えて言う（React の画面と同じ）
readEdited :: Text -> Either [Text] (Forest Item)
readEdited text = case concat problems of
  [] -> Right (fromOutline rows)
  ps -> Left ps
  where
    typed    = [ (n, l) | (n, l) <- zip [1 :: Int ..] (T.lines text)
                        , not (T.null (T.strip l)) ]
    rows     = [ (depthOf l, itemOf (T.strip l)) | (_, l) <- typed ]
    depths   = map fst rows
    problems = zipWith3 check typed depths (-1 : depths)
    check (n, l) depth above
      | spacesOf l `mod` 2 /= 0 =
          [at n l <> "：字下げは、空白2つずつにしてください"]
      | depth > above + 1 =
          [at n l <> "：上の行より、2段以上深くなっています"]
      | otherwise = []
    at n l = T.pack (show n) <> "行目「" <> T.strip l <> "」"
    spacesOf = T.length . T.takeWhile (== ' ')
    depthOf l = spacesOf l `div` 2

-- 🔎 One line: "#3 " at the front keeps a number; a number at the end
--    is the size (0 if there is none).
-- 1行："#3 " が頭にあれば番号を保つ。最後の数が量（なければ 0）
itemOf :: Text -> Item
itemOf line = Item number (T.strip heading) size
  where
    (number, rest) = case T.stripPrefix "#" line of
      Just after | (ds, more) <- T.span isDigit after, not (T.null ds) ->
        (Just (EntryId (read (T.unpack ds))), T.strip more)
      _ -> (Nothing, line)
    (heading, size) = case T.breakOnEnd " " rest of
      (front, lastWord) | not (T.null front), T.all isDigit lastWord
                        , not (T.null lastWord) ->
        (front, read (T.unpack lastWord))
      _ -> (rest, 0)
-- ANCHOR_END: text

-- ANCHOR: form
-- 🧾 The form: title, creator, and the lines. It replaces itself with
--    the answer (hx-target "this").
-- フォーム：題・作者・行。答えで自分を差し替える（hx-target "this"）
editForm :: Int -> Text -> Text -> Text -> [Text] -> Html ()
editForm n title creator lines' problems =
  form_ [ class_ "edit", hxPut ("/works/" <> T.pack (show n) <> "/edit")
        , hxTarget "this", hxSwap "outerHTML" ] $ do
    problemBox problems
    label_ ("題 "   <> input_ [name_ "title",   value_ title])
    label_ ("作者 " <> input_ [name_ "creator", value_ creator])
    label_ [class_ "outline"] $ do
      "目次（行の頭の #番号 はそのままに。新しい行は # なしで）"
      textarea_ [name_ "toc", rows_ "12"] (toHtml lines')
    button_ [class_ "primary"] "保存する"
-- ANCHOR_END: form

-- ANCHOR: app
type EditApi =
  "works" :> Capture "id" Int :> "edit" :>
    (    Get '[HTML] (Html ())
    :<|> ReqBody '[FormUrlEncoded] Fields :> Put '[HTML] (Html ()) )

-- 🌐 GET shows the form; PUT fixes the work through the window, and
--    answers the form again: fresh from the shelf, or as typed with
--    what is wrong (422).
-- GET はフォームを見せる。PUT は窓口を通して作品を直し、フォームを返す：
-- うまくいけば本棚から読み直したもの、だめなら打ったままと理由（422）
editApp :: Shelf -> Application
editApp shelf = serve (Proxy :: Proxy EditApi) $ \n -> fresh n [] :<|> save n
  where
    fresh n said = do
      found <- liftIO (shelf.works.getCard (WorkId n))
      case found of
        Nothing   -> throwError err404
        Just card -> do
          let d = detailOf (WorkId n, card)
          pure (editForm n d.work.title d.work.creator (tocText d.toc) said)
    save n fs = case readEdited (field "toc" fs) of
      Left ps   -> refused n fs ps
      Right toc -> do
        let r = Revision (field "title" fs) (field "creator" fs) toc
                         MoveToWork
        result <- liftIO (shelf.works.reviseWork (WorkId n) r)
        case result of
          Left mistake -> refused n fs (explain mistake)
          Right ()     -> fresh n []
    refused n fs ps = throwError err422
      { errBody = renderBS (editForm n (field "title" fs)
                              (field "creator" fs) (field "toc" fs) ps)
      , errHeaders = [("Content-Type", "text/html;charset=utf-8")] }
-- ANCHOR_END: app
-- ANCHOR_END: all
