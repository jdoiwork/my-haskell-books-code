-- ANCHOR: all
-- | Training: a search box that asks as you type.
-- トレーニング：打つたびに聞く、探す箱
module ShelfAnswers.LiveSearch (liveSearchBox) where

import Lucid
import Lucid.Base (makeAttribute)

import Tsundoku.Html.Parts (hxGet, hxTarget, hxTrigger)

-- 🔎 Ask /search when what is typed has changed and 300 ms have passed
--    with no more typing. An empty box asks nothing: it clears the
--    hits (the server would say "type some words" with 422).
-- 打ったものが変わって、300 ミリ秒打たなかったら /search に聞く。
-- 箱が空なら聞かずに、結果を消す（聞くと、サーバーは 422 で
-- 「探す言葉を入れてください」と言う）
liveSearchBox :: Html ()
liveSearchBox =
  input_ [ type_ "search", name_ "q", placeholder_ "本棚の中を探す"
         , hxGet "/search", hxTarget "#hits"
         , hxTrigger "input changed delay:300ms"
         , makeAttribute "hx-on::config-request" clearWhenEmpty ]
  where
    clearWhenEmpty =
      "if (this.value.trim() === '') {"
        <> " event.preventDefault();"
        <> " document.getElementById('hits').replaceChildren(); }"
-- ANCHOR_END: all
