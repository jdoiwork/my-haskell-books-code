-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE OverloadedRecordDot #-}
-- | The parts of the web pages, written with lucid: each part is a
--   function from what to show to HTML. They draw the same shapes the
--   API sends (WorkSummary, CardDetail, HitItem), so every screen draws
--   the same numbers. Nothing here asks the window.
-- Web の画面の部品。lucid で書く：部品の1つ1つが、見せるものから HTML への
-- 関数。API が送るのと同じ形（WorkSummary・CardDetail・HitItem）を描くので、
-- どの画面も同じ数を描く。ここには、窓口に聞くものは1つもない
module Tsundoku.Html.Parts
  ( -- * Pages / ページ
    page
  , shelfPage
  , cardPage
    -- * Parts that htmx swaps / htmx が差し替える部品
  , progressBar
  , ratingPart
  , readingPart
  , notePart
  , attrsPart
  , hitsPart
  , problemBox
  , Extras (..)
  , tagsPart
  , seenPart
  , alikePart
  , statsPage
    -- * htmx attributes / htmx の属性
  , hxGet
  , hxPost
  , hxPut
  , hxTarget
  , hxSwap
  , hxTrigger
  , hxVals
  , workUrl
  ) where

-- control, lists, maps, and text
-- 制御、リスト、辞書、文字列
import Control.Monad (forM_, unless)
import Data.Aeson (encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

-- HTML as Haskell functions
-- HTML を Haskell の関数で
import Lucid
import Lucid.Base (makeAttribute)

import Tsundoku.Api.Json
import Tsundoku.Form (FormRow (..), Widget (..))
import Tsundoku.Card (Card (rating, work))
import Tsundoku.Labels (eventLabel, stateLabel)
import Tsundoku.Review (Month (..), MonthStat (..), Seen (..))
import Tsundoku.Rating (RatingStyle, Score, marks, render, score, scoreValue,
                        snap, step)
import Tsundoku.Types (Pages (..), Work (title), WorkId (..))
-- ANCHOR_END: header

-- ANCHOR: htmx
-- 🧲 htmx is only attributes on HTML. lucid makes any attribute with
--    makeAttribute; give the ones we use names.
-- htmx は、HTML の属性だけ。lucid では、どんな属性も makeAttribute で
-- 作れる。使うものに名前を付けておく
hxGet, hxPost, hxPut, hxTarget, hxSwap, hxTrigger :: Text -> Attribute
hxGet     = makeAttribute "hx-get"      -- GET here when triggered
hxPost    = makeAttribute "hx-post"     -- POST here
hxPut     = makeAttribute "hx-put"      -- PUT here
hxTarget  = makeAttribute "hx-target"   -- where the answer goes
hxSwap    = makeAttribute "hx-swap"     -- how it goes in
hxTrigger = makeAttribute "hx-trigger"  -- when to send

-- 🧳 Values to send along, as JSON (hx-vals).
-- 一緒に送る値を、JSON で（hx-vals）
hxVals :: [(Text, Text)] -> Attribute
hxVals pairs = makeAttribute "hx-vals" (jsonText (object kvs))
  where
    kvs      = [ Key.fromText k .= v | (k, v) <- pairs ]
    jsonText = TE.decodeUtf8 . LBS.toStrict . encode

-- 🧭 The URL of a part of one work: workUrl 1 "rating" is /works/1/rating.
-- 作品1つの部品の URL：workUrl 1 "rating" は /works/1/rating
workUrl :: Int -> Text -> Text
workUrl wid part = "/works/" <> T.pack (show wid) <> "/" <> part
-- ANCHOR_END: htmx

-- ANCHOR: page
-- 📄 The frame of every page: htmx, the style, and a box for problems.
--    The meta tag tells htmx to show a 4xx answer in that box only
--    (swap "none": only the out-of-band problem box is swapped). When
--    no answer comes (a 5xx, or the server is down), the body's hx-on
--    says so in the same box.
-- どのページにも共通の枠：htmx、見た目、まずかったことの箱。
-- meta のタグは、4xx の答えをその箱にだけ出すように htmx に頼む
-- （swap は none：帯の外の problem の箱だけが差し替わる）。答えが来ない
-- とき（5xx、サーバーが止まっている）は、body の hx-on が同じ箱に書く
page :: Text -> Html () -> Html ()
page title body = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "htmx-config", content_ htmxConfig]
    title_ (toHtml title)
    link_ [rel_ "stylesheet", href_ "/static/shelf.css"]
    link_ [rel_ "stylesheet", href_ "/static/theme.css"]
    script_ [src_ "/static/htmx.min.js"] ("" :: Text)
  body_ [ makeAttribute "hx-on:htmx:response-error" noAnswer
        , makeAttribute "hx-on:htmx:send-error"     noAnswer ] $ do
    header_ (a_ [href_ "/"] "📚 tsundoku")
    problemBox []
    main_ body
  where
    htmxConfig = T.concat
      [ "{\"responseHandling\": ["
      , "{\"code\": \"204\", \"swap\": false},"
      , "{\"code\": \"[23]..\", \"swap\": true},"
      , "{\"code\": \"4..\", \"swap\": true, \"error\": false,"
      , " \"swapOverride\": \"none\"},"
      , "{\"code\": \"...\", \"swap\": false, \"error\": true}]}" ]
    -- Say so in the problem box, with the status (none: no answer).
    -- problem の箱に書く。答えの状態も（なし：答えが来なかった）
    noAnswer = T.concat
      [ "document.getElementById('problem').innerHTML ="
      , " '<ul><li>うまくいきませんでした（サーバーの答え：'"
      , " + (event.detail.xhr.status || 'なし')"
      , " + '）。少し待って、もう一度どうぞ</li></ul>'" ]

-- 🚨 The box for what went wrong. A part answers with it out of band
--    (hx-swap-oob), so it replaces the box wherever it is on the page.
--    An empty list clears the box.
-- まずかったことの箱。部品は、帯の外（hx-swap-oob）でこれを返すので、
-- ページのどこにあっても、この箱が差し替わる。空のリストなら箱を空に
problemBox :: [Text] -> Html ()
problemBox messages =
  div_ [id_ "problem", class_ "problem", makeAttribute "hx-swap-oob" "true"] $
    unless (null messages) $
      ul_ (forM_ messages (li_ . toHtml))
-- ANCHOR_END: page

-- ANCHOR: shelfPage
-- 📚 The shelf: a search box, and one row for each work.
-- 本棚：探す箱と、作品ごとに1行
shelfPage :: RatingStyle -> [WorkSummary] -> Html ()
shelfPage style works = page "本棚" $ do
  form_ [hxGet "/search", hxTarget "#hits"] $
    input_ [type_ "search", name_ "q", placeholder_ "本棚の中を探す"]
  div_ [id_ "hits"] ""
  ul_ [class_ "works"] (forM_ works (workRow style))

-- 📗 One row of the shelf: the title, the state, the rating, the bar.
-- 本棚の1行：題、状態、評価、バー
workRow :: RatingStyle -> WorkSummary -> Html ()
workRow style w = li_ $ do
  a_ [href_ ("/works/" <> T.pack (show w.id))] (toHtml w.title)
  span_ [class_ "creator"]  (toHtml w.creator)
  span_ [class_ "state"]    (toHtml (labelOf stateLabel w.reading))
  span_ [class_ "rating"]   (toHtml (shownRating style w.rating))
  maybe "" progressBar w.progress
-- ANCHOR_END: shelfPage

-- ANCHOR: bar
-- 📊 The bar of the progress: the numbers the core counted, drawn as
--    a <progress> element.
-- 進み具合のバー：核が数えた数を、<progress> の要素で描く
progressBar :: ProgressNumbers -> Html ()
progressBar p = span_ [class_ "progress"] $ do
  progress_ [value_ (shown p.done), max_ (shown p.total)] ""
  toHtml (" " <> shown p.percent <> "%")
  where
    shown n = T.pack (show (n :: Int))
-- ANCHOR_END: bar

-- ANCHOR: cardPage
-- 🗂️ The page of one work. Each part has its own id, so that htmx can
--    swap it alone.
-- 作品1つのページ。部品ごとに id があるので、htmx は1つだけを差し替えられる
cardPage :: RatingStyle -> [FormRow] -> Extras -> CardDetail -> Html ()
cardPage style rows extras card = page card.work.title $ do
  h1_ (toHtml card.work.title)
  p_ [class_ "creator"] (toHtml card.work.creator)
  div_ [id_ "bar"] (maybe "" progressBar card.work.progress)
  readingPart wid card.work.reading card.next
  ratingPart style wid card.work.rating
  tagsPart wid extras.tags
  unless (null card.toc) $ do
    h2_ "目次"
    tocPart card.toc
  h2_ "感想"
  noteForm wid card.toc
  ul_ [id_ "notes"] (forM_ (reverse extras.seen) (seenPart card.toc))
  unless (null extras.alike) (alikePart extras.alike)
  h2_ "自分で決めた属性と状態"
  attrsPart wid rows
  where
    wid = card.work.id
-- ANCHOR_END: cardPage

-- ANCHOR: extras
-- 🔖 What the review site adds to a card's page: the tags, the notes as
--    I may see them now, and works that look alike.
-- レビューサイトがカードのページに足すもの：タグ、いまの自分に見せてよい
-- 感想、似ている作品
data Extras = Extras
  { tags  :: Set Text
  , seen  :: [(Seen, Text)]        -- a note, and the hint if hidden
  , alike :: [(Int, Text)]         -- number and title / 番号と題
  }

-- 🔖 The tags, and a box to change them (commas between).
-- タグと、それを変える箱（カンマで区切る）
tagsPart :: Int -> Set Text -> Html ()
tagsPart wid ts =
  form_ [ id_ "tags", class_ "tags"
        , hxPost (workUrl wid "tags"), hxSwap "outerHTML" ] $ do
    forM_ (Set.toList ts) $ \t ->
      a_ [class_ "tag", href_ ("/?tag=" <> t)] (toHtml ("#" <> t))
    input_ [ type_ "text", name_ "tags", placeholder_ "タグ（カンマで区切る）"
           , value_ (T.intercalate ", " (Set.toList ts)) ]
    button_ "タグを保存"

-- 🙈 A note as I may see it: shown, or a hint of when it will show.
-- いまの自分に見せてよい感想：見せるか、いつ見えるかのヒント
seenPart :: [TocItem] -> (Seen, Text) -> Html ()
seenPart toc (s, hint) = case s of
  Shown note -> notePart toc (noteItem note)
  Hidden _   -> li_ [class_ "hidden"] (toHtml hint)

-- 🤝 Works that look alike, by their tags.
-- タグで似ている作品
alikePart :: [(Int, Text)] -> Html ()
alikePart works = do
  h2_ "この作品が好きなら"
  ul_ [class_ "alike"] $ forM_ works $ \(n, title) ->
    li_ (a_ [href_ ("/works/" <> T.pack (show n))] (toHtml title))
-- ANCHOR_END: extras

-- ANCHOR: statsPage
-- 📊 The statistics: month by month, the tag cloud, the best of a year.
-- 統計：月ごと、タグの雲、その年のベスト
statsPage :: RatingStyle -> Integer -> Map Month MonthStat -> [(Text, Int)]
          -> [(WorkId, Card)] -> Html ()
statsPage style y months cloud best = page "読書の統計" $ do
  h1_ "読書の統計"
  table_ [class_ "months"] $ do
    tr_ (th_ "月" <> th_ "読み終えた" <> th_ "ページ")
    forM_ (Map.toList months) $ \(Month yy mm, MonthStat n (Pages p)) ->
      tr_ $ do
        td_ (toHtml (T.pack (show yy) <> "年" <> T.pack (show mm) <> "月"))
        td_ (toHtml (T.pack (show n) <> "冊"))
        td_ $ do
          meter_ [value_ (shown p), max_ (shown (maximum (1 : pagesOf)))] ""
          toHtml (" " <> shown p)
  h2_ "タグ"
  p_ [class_ "cloud"] $ forM_ cloud $ \(t, n) ->
    a_ [ href_ ("/?tag=" <> t)
       , style_ ("font-size: " <> T.pack (show (100 + 20 * min 5 n)) <> "%") ]
       (toHtml ("#" <> t <> " "))
  h2_ (toHtml (T.pack (show y) <> "年のベスト"))
  ol_ $ forM_ best $ \(WorkId n, c) ->
    li_ $ do
      a_ [href_ ("/works/" <> T.pack (show n))] (toHtml c.work.title)
      toHtml (" " <> maybe "—" (render style) c.rating)
  where
    pagesOf = [ p | MonthStat _ (Pages p) <- Map.elems months ]
    shown n = T.pack (show (n :: Int))
-- ANCHOR_END: statsPage

-- ANCHOR: toc
-- 🌳 The table, as nested lists. What I have read gets a check.
-- 目次を入れ子のリストで。読み終えた項目にはチェック
tocPart :: [TocItem] -> Html ()
tocPart items = ul_ [class_ "toc"] $ forM_ items $ \item ->
  li_ $ do
    span_ [class_ (if item.done then "done" else "unread")]
          (toHtml (if item.done then "✓ " else "・" :: Text))
    toHtml item.heading
    unless (null item.children) (tocPart item.children)
-- ANCHOR_END: toc

-- ANCHOR: rating
-- ⭐ The rating, and how to change it. With a scale of marks (★ or
--    /10), one button for each mark; clicking sends that mark's points.
--    With a scale of 1 point, a box to type in. The box remembers what
--    it showed, so saving it untouched keeps the points as they were.
-- 評価と、その変え方。目盛りのある見せ方（★、/10）なら、目盛りごとに
-- ボタン。押すと、その目盛りの点を送る。1点刻みなら、打ちこむ箱。
-- 箱は、見せたものを覚えておく。さわらずに保存しても、点は変わらない
ratingPart :: RatingStyle -> Int -> Maybe Int -> Html ()
ratingPart style wid points =
  form_ [ id_ "rating", class_ "rating"
        , hxPut (workUrl wid "rating"), hxSwap "outerHTML" ] $
    if step style > 1
      then forM_ (marks style) markButton
      else do
        input_ [type_ "text", name_ "typed", value_ shown]
        input_ [type_ "hidden", name_ "shown", value_ shown]
        button_ "保存"
  where
    now   = points >>= scoreOf
    shown = maybe "" (render style) now
    markButton m =
      button_ ( [name_ "points", value_ (T.pack (show (scoreValue m)))]
                ++ [class_ "now" | Just m == fmap (snap style) now] )
              (toHtml (render style m))

-- 🔢 The points the API sends, as a Score (Nothing if out of range).
-- API が送る点を Score に（範囲の外なら Nothing）
scoreOf :: Int -> Maybe Score
scoreOf n = either (const Nothing) Just (score n)

-- ⭐ The rating as a word, or a dash when there is none.
-- 評価を言葉に。なければダッシュ
shownRating :: RatingStyle -> Maybe Int -> Text
shownRating style points = maybe "—" (render style) (points >>= scoreOf)
-- ANCHOR_END: rating

-- ANCHOR: reading
-- 📖 The reading state, and a button for each thing I can do now.
--    The buttons come from `next`; the page knows no map of states.
-- 読書の状態と、いまできることのボタン。ボタンは next から作る。
-- ページは、状態の地図を知らない
readingPart :: Int -> Text -> [Text] -> Html ()
readingPart wid reading next =
  form_ [ id_ "reading", class_ "reading"
        , hxPost (workUrl wid "reading"), hxSwap "outerHTML" ] $ do
    span_ [class_ "state"] (toHtml (labelOf stateLabel reading))
    forM_ next $ \event ->
      button_ [name_ "event", value_ event]
              (toHtml (labelOf eventLabel event))
-- ANCHOR_END: reading

-- ANCHOR: note
-- ✍️ The form for a new note. The answer (one <li>) goes in at the top
--    of the list (afterbegin), and the form is cleared.
-- 新しい感想のフォーム。答え（<li> が1つ）は、リストの先頭に入る
-- （afterbegin）。フォームは空にする
noteForm :: Int -> [TocItem] -> Html ()
noteForm wid toc =
  form_ [ class_ "note-form"
        , hxPost (workUrl wid "notes"), hxTarget "#notes"
        , hxSwap "afterbegin"
        , makeAttribute "hx-on::after-request"
                         "if (event.detail.successful) this.reset()" ] $ do
    textarea_ [name_ "body", rows_ "3"] ""
    select_ [name_ "entry"] $ do
      option_ [value_ ""] "作品の全体"
      forM_ (flatItems toc) $ \item ->
        option_ [value_ (T.pack (show item.id))] (toHtml item.heading)
    label_ $ do
      input_ [type_ "checkbox", name_ "spoiler"]
      "ネタバレ"
    button_ "書く"

-- 📝 One note. A spoiler is folded in <details>; the reader opens it.
-- 感想1つ。ネタバレは <details> に畳む。開くのは読む人
notePart :: [TocItem] -> NoteItem -> Html ()
notePart toc note = li_ $ do
  span_ [class_ "where"] (toHtml (whereOf note.entry))
  time_ (toHtml (T.take 10 note.writtenAt))
  if note.spoiler
    then details_ $ do
      summary_ "ネタバレ"
      p_ (toHtml note.body)
    else p_ (toHtml note.body)
  where
    whereOf Nothing  = "作品の全体"
    whereOf (Just e) =
      case [ item.heading | item <- flatItems toc, item.id == e ] of
        heading : _ -> heading
        []          -> "（目次にない項目）"

-- 🌳 Every item of the table, from the top.
-- 目次の項目の全部を、上から順に
flatItems :: [TocItem] -> [TocItem]
flatItems = concatMap (\item -> item : flatItems item.children)
-- ANCHOR_END: note

-- ANCHOR: attrs
-- 🎛️ The form of my own attributes and states, row by row, as the
--    definitions say. Only the drawing is here: the rows come from
--    Tsundoku.Form, which knows nothing of HTML.
-- 自分で決めた属性と状態のフォーム。定義のとおり、1行ずつ。ここにあるのは
-- 描き方だけ：行は Tsundoku.Form から来る（そちらは HTML を知らない）
attrsPart :: Int -> [FormRow] -> Html ()
attrsPart wid rows =
  div_ [id_ "attrs"] $ do
    form_ [ hxPost (workUrl wid "attrs"), hxTarget "#attrs"
          , hxSwap "outerHTML" ] $ do
        forM_ [ r | r <- rows, not (isButtons r.widget) ] attrRow
        button_ "属性を保存"
    forM_ [ (r, ns) | r <- rows, Buttons ns <- [r.widget] ] flowRow
  where
    isButtons w = case w of
      Buttons _ -> True
      _         -> False
    flowRow (row, next) = p_ [class_ "flow"] $ do
      toHtml (row.label <> "：" <> nowOr row.current <> " ")
      forM_ next $ \to ->
        button_ [ hxPost (workUrl wid "flow"), hxTarget "#attrs"
                , hxSwap "outerHTML"
                , hxVals [("flow", row.label), ("to", to)] ]
                (toHtml ("→ " <> to))
    nowOr ""  = "（まだ入っていない）"
    nowOr now = now

-- 🎛️ One row of attributes: a label and the widget the kind asks for.
-- 属性の1行：見出しと、種類が求める入力欄
attrRow :: FormRow -> Html ()
attrRow row = label_ [class_ "attr"] $ do
  span_ (toHtml row.label)
  case row.widget of
    TextBox     -> box "text"
    NumberBox   -> box "number"
    DateBox     -> box "date"
    CheckBox    -> do
      -- an unchecked box sends nothing; the hidden "" says "no value"
      -- チェックのない箱は何も送らない。隠した "" が「値なし」を伝える
      input_ [type_ "hidden", name_ row.label, value_ ""]
      input_ ( [type_ "checkbox", name_ row.label, value_ "はい"]
               ++ [checked_ | row.current == "はい"] )
    Select cs   -> select_ [name_ row.label] $ do
      option_ [value_ ""] "—"
      forM_ cs $ \c ->
        option_ ([value_ c] ++ [selected_ "selected" | c == row.current])
                (toHtml c)
    Buttons _   -> ""
  where
    box kind = input_ [type_ kind, name_ row.label, value_ row.current]
-- ANCHOR_END: attrs

-- ANCHOR: hits
-- 🔎 What was found. The words that matched are <mark>ed here, by the
--    screen; the API only said which pieces matched.
-- 見つかったもの。当たった語に <mark> を付けるのは、ここ（画面）。
-- API は、どのかけらが当たったかを言っただけ
hitsPart :: [HitItem] -> Html ()
hitsPart [] = p_ [class_ "hits"] "見つかりませんでした"
hitsPart hits = ol_ [class_ "hits"] $ forM_ hits $ \hit -> li_ $ do
  a_ [href_ ("/works/" <> T.pack (show hit.work))] (toHtml hit.title)
  unless (null hit.trail) $
    span_ [class_ "trail"] (toHtml (" > " <> T.intercalate " > " hit.trail))
  p_ $ forM_ hit.snippet $ \snip ->
    if snip.match then mark_ (toHtml snip.text) else toHtml snip.text
-- ANCHOR_END: hits

-- ANCHOR: labelOf
-- 🏷️ A word from the API (a constructor's name), as a person reads it.
-- API の言葉（コンストラクターの名前）を、人が読む言葉に
labelOf :: (Enum a, Bounded a, Show a) => (a -> Text) -> Text -> Text
labelOf name word = maybe word name (fromName word)
-- ANCHOR_END: labelOf
-- ANCHOR_END: all
