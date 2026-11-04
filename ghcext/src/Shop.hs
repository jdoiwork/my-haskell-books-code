{-# LANGUAGE OrPatterns #-}
-- ANCHOR: all
-- | Patterns: "this or that" in one line, and \case.
-- パターン：「これか、あれ」を1行で。そして \case
module Shop
  ( category
  , categoryOld
  , describe
  ) where

-- ANCHOR: or
-- 🛒 Without OrPatterns: one line for each item.
-- OrPatterns なし：アイテムごとに1行
categoryOld :: String -> String
categoryOld item = case item of
  "potion" -> "medicine"
  "elixir" -> "medicine"
  "sword"  -> "weapon"
  "axe"    -> "weapon"
  _        -> "other"

-- 🛒 With OrPatterns: "this or that" in one pattern, joined by ;.
-- OrPatterns あり：「これか、あれ」を、; でつないで1つのパターンに
category :: String -> String
category item = case item of
  ("potion"; "elixir") -> "medicine"
  ("sword"; "axe")     -> "weapon"
  _                    -> "other"
-- ANCHOR_END: or

-- ANCHOR: lambdacase
-- 🧾 \case: a lambda that goes straight into a case (GHC2024 has it).
-- \case：すぐに case をするラムダ（GHC2024 に入っている）
describe :: [Maybe Int] -> [String]
describe = map $ \case
  Nothing -> "sold out"
  Just n  -> show n ++ " G"
-- ANCHOR_END: lambdacase
-- ANCHOR_END: all
