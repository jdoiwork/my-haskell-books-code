-- ANCHOR: all
-- | Training: a search box that asks as you type.
-- トレーニング：打つたびに聞く、探す箱
module ShelfAnswers.LiveSearch (liveSearchBox) where

import Lucid

import Tsundoku.Html.Parts (hxGet, hxTarget, hxTrigger)

-- 🔎 Ask /search when what is typed has changed and 300 ms have passed
--    with no more typing, or when the box is cleared (the "search"
--    event of <input type="search">).
-- 打ったものが変わって、300 ミリ秒打たなかったら /search に聞く。
-- 箱を空にしたとき（<input type="search"> の search のできごと）も
liveSearchBox :: Html ()
liveSearchBox =
  input_ [ type_ "search", name_ "q", placeholder_ "本棚の中を探す"
         , hxGet "/search", hxTarget "#hits"
         , hxTrigger "input changed delay:300ms, search" ]
-- ANCHOR_END: all
