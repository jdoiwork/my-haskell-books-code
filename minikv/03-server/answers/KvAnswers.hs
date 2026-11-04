-- ANCHOR: all
-- | Answers to the training of "Design: values and commands as types".
-- 「設計：値とコマンドを型で描く」のトレーニングの解答
module KvAnswers
  ( getSet
  , kindCounts
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import MiniKv.Store
import MiniKv.Types

-- ANCHOR: getSet
-- 🔁 Step 2: store a new text, and answer with the old one.
--    Built from two commands; WrongType comes from GET.
-- ステップ2：新しい文字列をしまい、古いほうを答える。
-- 2つのコマンドの組み合わせ。WrongType は GET が出してくれる
getSet :: Key -> Text -> Store -> (Reply, Store)
getSet key text store =
  case apply (Get key) store of
    (Failed err, _) -> (Failed err, store)
    (old, _)        -> (old, snd (apply (Set key text) store))
-- ANCHOR_END: getSet

-- ANCHOR: kinds
-- 🏷️ Step 3: how many values of each kind are stored.
-- ステップ3：それぞれの種類の値が、いくつあるか
kindCounts :: Store -> [(Kind, Int)]
kindCounts store =
  let kinds = [ (kindOf v, 1) | v <- storedValues store ]
  in  Map.toList (Map.fromListWith (+) kinds)
-- ANCHOR_END: kinds
-- ANCHOR_END: all
