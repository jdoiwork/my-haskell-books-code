-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE OverloadedRecordDot #-}
-- | The terminal screen's model, in the Elm way: a state, the keys, and
--   what a key does to the state. Asking the API is not done here; a key
--   only says what to ask (Ask), and an answer comes back (Answer).
--   All pure, so it can be tested without a terminal or a server.
-- ターミナルの画面のモデル。Elm 風に：状態、キー、キーで状態がどう変わるか。
-- API に聞くのは、ここではしない。キーは何を聞くか（Ask）を言うだけで、
-- 答え（Answer）があとから届く。全部が純粋なので、端末もサーバーもなしに
-- テストできる
module Tsundoku.Tui.Model
  ( Model (..)
  , Screen (..)
  , Key (..)
  , Ask (..)
  , Answer (..)
  , initial
  , step
  , arrive
  , selected
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Tsundoku.Api.Json
-- ANCHOR_END: header

-- ANCHOR: model
-- 🧠 Everything the screen shows comes from here.
-- 画面に出るものは、全部ここから
data Model = Model
  { works   :: [WorkSummary]   -- the shelf / 本棚
  , cursor  :: Int             -- which row is chosen / 選んでいる行
  , screen  :: Screen          -- what is shown / 見せているもの
  , message :: Text            -- a word at the bottom / 下の行の言葉
  }
  deriving (Show, Eq)

-- 🖥️ The shelf, or one card opened (with the rows of my own attributes).
-- 本棚か、開いたカード1枚（自分で決めた属性の行も）
data Screen = OnShelf | OnCard CardDetail [FormField]
  deriving (Show, Eq)

-- ⌨️ The keys the screen knows (the runner turns real keys into these).
-- 画面が知っているキー（本物のキーは、動かす側がこれに直す）
data Key
  = Down          -- j, ↓
  | Up            -- k, ↑
  | Open          -- Enter
  | Back          -- Esc, h
  | Pick Int      -- 1..9: the n-th thing to do / n 番目のできること
  | More          -- +: half a star more / ★を半分ふやす
  | Less          -- -: half a star less / ★を半分へらす
  | QuitKey       -- q
  deriving (Show, Eq)

-- 📮 What to ask the API. The runner asks, in order.
-- API に何を聞くか。動かす側が、順に聞く
data Ask
  = LoadWorks
  | LoadCard Int
  | Happen   Int Text     -- work, event's name / 作品、できごとの名前
  | Rate     Int Int      -- the work, and points / 作品と点
  | Quit
  deriving (Show, Eq)

-- 📬 What came back.
-- 返ってきたもの
data Answer
  = Works   [WorkSummary]
  | Card    CardDetail [FormField]
  | Done
  | Refused Problem
  | Offline Text
  deriving (Show, Eq)

-- 🌱 The screen before anything has come.
-- まだ何も届いていない画面
initial :: Model
initial = Model { works = [], cursor = 0, screen = OnShelf, message = "" }
-- ANCHOR_END: model

-- ANCHOR: step
-- 👣 What a key does: a new model, and what to ask. One row for each
--    screen and key; any other pair does nothing.
-- キーがすること：新しいモデルと、聞くこと。画面とキーの組ごとに1行。
-- ほかの組は、何もしない
step :: Key -> Model -> (Model, [Ask])
step key before = case (model.screen, key) of
  (_,          QuitKey) -> (model, [Quit])
  (OnShelf,    Down   ) -> (move 1,    [])
  (OnShelf,    Up     ) -> (move (-1), [])
  (OnShelf,    Open   ) -> (model,     maybe [] (\w -> [LoadCard w.id])
                                             (selected model))
  (OnCard _ _, Back   ) -> (model { screen = OnShelf }, [LoadWorks])
  (OnCard c _, Pick n ) -> (model, pick c n)
  (OnCard c _, More   ) -> (model, rateBy c 10)
  (OnCard c _, Less   ) -> (model, rateBy c (-10))
  _                     -> (model, [])
  where
    -- a key starts a new turn: the last word on the bottom line goes
    -- キーが来たら、新しい1手。下の行の前の言葉は消す
    model  = before { message = "" }
    move d = model { cursor = clamp 0 (length model.works - 1)
                                    (model.cursor + d) }
    pick c n = case drop (n - 1) c.next of
      event : _ | n >= 1 -> [Happen c.work.id event, LoadCard c.work.id]
      _                  -> []
    rateBy c d =
      let now = fromMaybe 50 c.work.rating
      in  [Rate c.work.id (clamp 0 100 (now + d)), LoadCard c.work.id]

-- 📏 Keep a number between two others.
-- 数を、2つの数のあいだに収める
clamp :: Int -> Int -> Int -> Int
clamp lo hi = max lo . min hi

-- 👉 The work on the chosen row, if there is one.
-- 選んでいる行の作品（あれば）
selected :: Model -> Maybe WorkSummary
selected model = case drop model.cursor model.works of
  w : _ -> Just w
  []    -> Nothing
-- ANCHOR_END: step

-- ANCHOR: arrive
-- 📬 What an answer does to the model. A refusal stays on the bottom
--    line even when the card is read again right after it (the next
--    key clears it, in step).
-- 答えが、モデルをどう変えるか。断りの言葉は、すぐあとにカードを
-- 読み直しても残す（消すのは、次のキー。step で）
arrive :: Answer -> Model -> Model
arrive answer model = case answer of
  Works ws    -> model { works  = ws
                       , cursor = clamp 0 (length ws - 1) model.cursor }
  Card c fs   -> model { screen = OnCard c fs }
  Done        -> model
  Refused p   -> model { message = mconcat p.messages }
  Offline why -> model { message = why }
-- ANCHOR_END: arrive
-- ANCHOR_END: all
