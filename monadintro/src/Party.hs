-- ANCHOR: all
-- 👥 The list as a Monad: every combination.
-- Monad としてのリスト：すべての組み合わせ
module Party (pairsComprehension, pairsDo) where

-- ANCHOR: party
-- 👥 Every pair of a front fighter and a back caster
--    (a list comprehension, as in the introduction).
-- 前衛と後衛の、すべての組（「はじめに」のリスト内包表記で）
pairsComprehension :: [(String, String)]
pairsComprehension =
  [ (front, back) | front <- ["Rust", "Lambda"], back <- ["Python", "Ruby"] ]

-- 👥 The same, with do: for a list, (>>=) tries every element.
-- 同じものを、do で：リストの (>>=) は、要素を1つずつ全部試す
pairsDo :: [(String, String)]
pairsDo = do
  front <- ["Rust", "Lambda"]
  back  <- ["Python", "Ruby"]
  pure (front, back)
-- ANCHOR_END: party
-- ANCHOR_END: all
