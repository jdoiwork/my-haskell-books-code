-- ANCHOR: all
-- | A view: the form for my own attributes and states, built from the
--   definitions, and words for what went wrong. Every screen (htmx,
--   React, brick) can draw these rows its own way.
-- ビュー：自分で決めた属性と状態のフォーム。定義から組み立てる。
-- それと、まずかったことの言葉。どの画面（htmx・React・brick）も、
-- この行を自分のやり方で描ける
-- ANCHOR: header
module Tsundoku.Form
  ( Widget (..)
  , FormRow (..)
  , formFor
  , formLines
  , explain
  ) where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Tsundoku.Card (Card (..), stateOf)
import Tsundoku.Custom
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: form
-- 🎛️ What to draw for one row of the form.
-- フォームの1行に、何を描くか
data Widget
  = TextBox              -- <input type="text">
  | NumberBox            -- <input type="number">
  | DateBox              -- <input type="date">
  | CheckBox             -- <input type="checkbox">
  | Select  [Text]       -- <select> with the choices / 選択肢
  | Buttons [Text]       -- a button for each next state / 次の状態のボタン
  deriving (Show, Eq)

-- 📋 One row: the label, what to draw, and what is in it now.
-- 1行：見出し、描くもの、いま入っているもの
data FormRow = FormRow
  { label   :: Text
  , widget  :: Widget
  , current :: Text
  }
  deriving (Show, Eq)

-- 🧱 The form of a card: a row for each attribute, then for each flow.
--    The definitions decide the rows; nothing here knows 推し度.
-- カードのフォーム：属性ごとに1行、続けて流れごとに1行。
-- 行を決めるのは定義。ここには、推し度を知っている行は1つもない
formFor :: Schema -> Card -> [FormRow]
formFor schema card =
  [ FormRow (attrName def) (widgetFor (attrKind def)) (valueOf def)
  | def <- attrDefs schema ]
  ++
  [ FormRow (flowName flow) (Buttons (nextStates flow now)) (fromMaybe "" now)
  | flow <- flows schema
  , let now = stateOf flow card ]
  where
    valueOf def =
      maybe "" showValue (Map.lookup (attrName def) (attrs card))

-- 🎛️ Each kind has its own widget.
-- 種類ごとに、描くものが決まっている
widgetFor :: AttrKind -> Widget
widgetFor kind = case kind of
  TextKind           -> TextBox
  NumberKind         -> NumberBox
  DateKind           -> DateBox
  YesNoKind          -> CheckBox
  ChoiceKind choices -> Select choices

-- 🖨️ The form as lines of text, for the terminal (and for GHCi).
-- フォームを文字の行に（端末と GHCi のため）
formLines :: [FormRow] -> [Text]
formLines = map line
  where
    line row = label row <> "：" <> drawn (widget row) (current row)
    drawn w now = case w of
      TextBox      -> box now
      NumberBox    -> box now <> "（数）"
      DateBox      -> box now <> "（日付）"
      CheckBox     -> if now == "はい" then "[x]" else "[ ]"
      Select cs    -> T.unwords [ mark now c | c <- cs ]
      Buttons next -> notYet now <> buttons next
    box now     = "[ " <> now <> " ]"
    notYet ""   = "（まだ入っていない）"
    notYet now  = now
    mark now c  = if c == now then "(" <> c <> ")" else c
    buttons []  = "（ここで終わり）"
    buttons ns  = "  → " <> T.intercalate " / " ns
-- ANCHOR_END: form

-- ANCHOR: explain
-- 💬 What went wrong, in words for the person at the screen.
-- まずかったことを、画面の前の人に向けた言葉で
explain :: Mistake -> [Text]
explain mistake = case mistake of
  BadScore n         -> ["点は 0〜100 です：" <> T.pack (show n)]
  BadRating typed    -> ["評価として読めません：" <> typed]
  NotAllowed _ _     -> ["いまの読書の状態では、それはできません"]
  NoSuchEntry _      -> ["目次にない項目です"]
  EmptyNote          -> ["感想が空っぽです"]
  NoSuchWork _       -> ["本棚にない作品です"]
  BadDefinition ps   -> map defProblem ps
  BadAttrs ps        -> map attrProblem ps
  NoSuchFlow name    -> ["「" <> name <> "」という状態の流れはありません"]
  NoSuchArrow f a b  -> [f <> "：" <> from a <> "「" <> b <> "」へ行けません"]
  CannotParse why    -> "絞り込みの言葉が読めません" : T.lines why
  NotASerial         -> ["連載でない作品に、話は足せません"]
  where
    from Nothing  = "まだ入っていないので、"
    from (Just a) = "「" <> a <> "」から"
    defProblem p = case p of
      NameTaken n   -> "「" <> n <> "」という名前は、もう使っています"
      NothingToPick -> "選択肢が1つもありません"
      Twice x       -> "「" <> x <> "」が2つあります"
      NoSuchState x -> "矢印の先か元の「" <> x <> "」が、状態にありません"
      Unreachable x -> "はじめの状態から「" <> x <> "」へ行けません"
      BadName x     -> "「" <> x <> "」は、名前や選択肢に使えません"
    attrProblem p = case p of
      UnknownName n  -> "「" <> n <> "」という属性も状態もありません"
      Unreadable n x -> n <> " に「" <> x <> "」は入りません"
      Unordered n    -> n <> " は、大きい・小さいで比べられません"
-- ANCHOR_END: explain
-- ANCHOR_END: all
