-- ANCHOR: head
-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | "Did you mean ...?": suggest a known word for a mistyped one.
-- 「もしかして」：打ちまちがえた単語に、知っている単語を勧める。
module Review.Suggest
  ( editDistance
  , suggest
  , suggestNaive
  , Answer (..)
  , answer
  ) where

import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T

import MiniSearch.Search (search)
import MiniSearch.Tokenizer (Tokenizer (..), english)
import MiniSearch.Types
-- ANCHOR_END: head

-- ANCHOR: distance
-- 📏 The edit distance: how many one-letter changes (add, remove,
--    replace) turn a into b. "dargon" -> "dragon" is 2.
-- 編集距離：1文字の変更（足す・消す・置きかえる）を何回すれば
-- a が b になるか。"dargon" -> "dragon" は 2
editDistance :: Text -> Text -> Int
editDistance a b = last (foldl' nextRow firstRow (T.unpack a))
  where
    bs = T.unpack b
    -- The distances from "" to each prefix of b: 0, 1, 2, ...
    -- 空の文字列から、b の先頭の部分までの距離：0, 1, 2, ...
    firstRow = [0 .. length bs]
    -- One more letter c of a: the next row, from the row above.
    -- a の文字 c を1つ進める：上の行から、次の行を作る
    nextRow above c =
      let start = head' above + 1
          cells = zip3 bs above (drop 1 above)
      in scanl (cell c) start cells
    cell c left (bc, diagonal, up) =
      minimum [ up + 1                                     -- remove
              , left + 1                                   -- add
              , diagonal + (if bc == c then 0 else 1) ]    -- replace
    head' (x : _) = x
    head' []      = 0
-- ANCHOR_END: distance

-- ANCHOR: suggest
-- 💡 The known word closest to w (distance 1 or 2).
--    Closest first; then the one in more documents; then a to z.
-- w にいちばん近い、知っている単語（距離 1 か 2）。
-- 近い順。同じなら、多くの文書に出てくる順。それも同じなら、辞書順
suggest :: Index -> Text -> Maybe Text
suggest index w =
  let candidates =
        [ (t, d, Map.size ps)
        | (Token t, ps) <- Map.toList (indexPostings index)
          -- Skip words whose length is too different (cheap check first).
          -- 長さが違いすぎる単語は、計算する前に飛ばす（安い確認が先）
        , abs (T.length t - T.length w) <= 2
        , let d = editDistance w t
        , d >= 1 && d <= 2 ]
  in case sortOn (\(t, d, df) -> (d, Down df, t)) candidates of
       ((t, _, _) : _) -> Just t
       []              -> Nothing

-- 🐢 The same without the length check (for measuring).
-- 長さの確認なしの版（測るため）
suggestNaive :: Index -> Text -> Maybe Text
suggestNaive index w =
  let candidates =
        [ (t, d, Map.size ps)
        | (Token t, ps) <- Map.toList (indexPostings index)
        , let d = editDistance w t
        , d >= 1 && d <= 2 ]
  in case sortOn (\(t, d, df) -> (d, Down df, t)) candidates of
       ((t, _, _) : _) -> Just t
       []              -> Nothing
-- ANCHOR_END: suggest

-- ANCHOR: answer
-- ANCHOR: answertype
-- 🗨️ What the search engine says back.
-- 検索エンジンの返事
data Answer
  = Found [Hit]       -- hits / 当たった
  | DidYouMean Text   -- もしかして
  | NothingFound      -- 何もない
  deriving (Eq, Show)
-- ANCHOR_END: answertype

-- 🔍 Search; if nothing is found, try fixing each unknown word.
--    The words are cut by the tokenizer search uses (english).
-- 検索する。何もなければ、知らない単語を1つずつ直してみる。
-- 単語は、search と同じトークナイザー（english）で切る
answer :: Index -> Text -> Answer
answer index query =
  case search index query of
    hits@(_ : _) -> Found hits
    [] -> case traverse fix [w | Token w <- cut english query] of
      Just ws
        | let fixed = T.unwords ws
        , not (null (search index fixed)) -> DidYouMean fixed
      _ -> NothingFound
  where
    known w = Map.member (Token w) (indexPostings index)
    fix w
      | known w   = Just w
      | otherwise = suggest index w
-- ANCHOR_END: answer
-- ANCHOR_END: all
