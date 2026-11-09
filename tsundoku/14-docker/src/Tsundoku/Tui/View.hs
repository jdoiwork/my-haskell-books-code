-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE OverloadedRecordDot #-}
-- | The terminal screen's view: the model drawn with brick. The lines of
--   text (the bar, the table, the notes) are plain functions, so the
--   tests read them without a terminal.
-- ターミナルの画面のビュー：モデルを brick で描く。文字の行（バー、目次、
-- 感想）は、ふつうの関数なので、テストは端末なしに読める
module Tsundoku.Tui.View
  ( draw
  , theme
  , barText
  , starsText
  , workLine
  , padTo
  , tocLines
  , noteLines
  , fieldLines
  , nextLine
  ) where

import Data.Text (Text)
import Data.Text qualified as T

-- brick: widgets, and their colors
-- brick：ウィジェットと、その色
import Brick
import Brick.Widgets.Border (borderWithLabel)
import Graphics.Text.Width (safeWcswidth)
import Graphics.Vty qualified as Vty

import Tsundoku.Api.Json
import Tsundoku.Labels (eventLabel, stateLabel)
import Tsundoku.Rating (RatingStyle (..), score)
import Tsundoku.Rating qualified as Rating
import Tsundoku.Tui.Model
-- ANCHOR_END: header

-- ANCHOR: draw
-- 🖥️ The whole screen: what is shown, and a line at the bottom.
-- 画面の全体：見せているものと、下の1行
draw :: Model -> [Widget ()]
draw model = [vBox [body, hint, withAttr problemAttr (txt model.message)]]
  where
    (body, hint) = case model.screen of
      OnShelf     -> (shelfBox model, txt "j/k：動く  Enter：開く  q：終わる")
      OnCard c fs -> (cardBox c fs,
                      txt "数字：できること  +/-：★  Esc：本棚へ  q：終わる")

-- 📚 The shelf: one line for each work; the chosen one stands out.
-- 本棚：作品ごとに1行。選んでいる行は目立たせる
shelfBox :: Model -> Widget ()
shelfBox model = borderWithLabel (txt " 本棚 ") $ vBox
  [ (if i == model.cursor then withAttr chosen else id) (txt (workLine w))
  | (i, w) <- zip [0 ..] model.works ]

-- 🗂️ One card: the bar, the state and what I can do, the stars, the
--    table, the notes, my own attributes.
-- カード1枚：バー、状態とできること、★、目次、感想、自分で決めた属性
cardBox :: CardDetail -> [FormField] -> Widget ()
cardBox c fs = borderWithLabel (txt (" " <> c.work.title <> " ")) $ vBox $
  [ txt (maybe "" (barText 20) c.work.progress)
  , txt (nextLine c.work.reading c.next)
  , txt ("評価 " <> starsText c.work.rating)
  , txt " " ]
  ++ map txt (tocLines c.toc)
  ++ [txt " "] ++ map txt (noteLines c.toc c.notes)
  ++ [txt " "] ++ map txt (fieldLines fs)
-- ANCHOR_END: draw

-- ANCHOR: lines
-- 📊 The bar of the progress, drawn with characters, from the numbers
--    the API sent (the same numbers the browser draws).
-- 進み具合のバーを文字で。API が送った数から（ブラウザーと同じ数）
barText :: Int -> ProgressNumbers -> Text
barText width p = T.replicate filled "▓" <> T.replicate (width - filled) "░"
               <> " " <> T.justifyRight 4 ' ' (T.pack (show p.percent) <> "%")
  where
    filled
      | p.total <= 0 = 0
      | otherwise    = min width (p.done * width `div` p.total)

-- ⭐ The points as stars, or a dash.
-- 点を★に。なければダッシュ
starsText :: Maybe Int -> Text
starsText points = case points of
  Nothing -> "—"
  Just n  -> either (const "—") (Rating.render FiveStars) (score n)

-- 📗 One line of the shelf.
-- 本棚の1行
workLine :: WorkSummary -> Text
workLine w = T.intercalate "  "
  [ padTo 6 (stateWord w.reading)
  , padTo 4 (starsText w.rating)
  , w.title ]

-- 📏 Pad to a width on the screen. A full-width character (漢字, ★)
--    takes 2 columns, so T.justifyLeft, which counts characters, would
--    not line up.
-- 画面の幅で詰める。全角の文字（漢字、★）は2桁ぶんなので、文字を数える
-- T.justifyLeft では、そろわない
padTo :: Int -> Text -> Text
padTo n t = t <> T.replicate (n - safeWcswidth (T.unpack t)) " "

-- 📖 The state, and the things I can do now, numbered for the keys.
-- 状態と、いまできること。キーのための番号つき
nextLine :: Text -> [Text] -> Text
nextLine reading next = T.intercalate "  " $
  stateWord reading : [ T.pack (show i) <> " " <> eventWord e
                      | (i, e) <- zip [1 :: Int ..] next ]

-- 🌳 The table, one line for each entry, indented by depth.
-- 目次。項目ごとに1行、深さで字下げ
tocLines :: [TocItem] -> [Text]
tocLines = concatMap (go 0)
  where
    go depth item =
      (T.replicate depth "  " <> mark item.done <> item.heading)
        : concatMap (go (depth + 1)) item.children
    mark True  = "✓ "
    mark False = "・"

-- 📝 The notes, newest first. A spoiler shows only that it is there.
-- 感想を新しい順に。ネタバレは、あることだけを見せる
noteLines :: [TocItem] -> [NoteItem] -> [Text]
noteLines toc notes = [ line n | n <- reverse notes ]
  where
    line n = "「" <> placeOf n.entry <> "」 "
          <> (if n.spoiler then "（ネタバレ。ブラウザーで読む）" else n.body)
    placeOf Nothing  = "作品の全体"
    placeOf (Just e) = headOr "（目次にない項目）"
                         [ i.heading | i <- flat toc, i.id == e ]
    flat = concatMap (\i -> i : flat i.children)
    headOr d xs = case xs of
      x : _ -> x
      []    -> d

-- 🎛️ My own attributes and states, as the definitions say. The
--    terminal only shows them; the browser edits them.
-- 自分で決めた属性と状態を、定義のとおりに。ターミナルは見せるだけ、
-- 書き換えるのはブラウザー
fieldLines :: [FormField] -> [Text]
fieldLines fs = [ f.label <> "：" <> shown f | f <- fs ]
  where
    shown f = case (f.kind, f.current) of
      ("yesno", "はい") -> "[x]"
      ("yesno", _     ) -> "[ ]"
      (_,       ""    ) -> "—"
      (_,       now   ) -> now
-- ANCHOR_END: lines

-- ANCHOR: theme
-- 🎨 The colors: the chosen row reversed, a refusal in red.
-- 色：選んでいる行は反転、断りの言葉は赤
theme :: AttrMap
theme = attrMap Vty.defAttr
  [ (chosen,      Vty.withStyle     Vty.defAttr Vty.reverseVideo)
  , (problemAttr, Vty.withForeColor Vty.defAttr Vty.red) ]

chosen, problemAttr :: AttrName
chosen      = attrName "chosen"
problemAttr = attrName "problem"
-- ANCHOR_END: theme

-- 🏷️ Words from the API, as a person reads them.
-- API の言葉を、人の読む言葉に
stateWord, eventWord :: Text -> Text
stateWord word = maybe word stateLabel (fromName word)
eventWord word = maybe word eventLabel (fromName word)
-- ANCHOR_END: all
