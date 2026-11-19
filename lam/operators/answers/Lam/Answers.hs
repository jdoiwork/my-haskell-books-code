{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The answer of the training: the table of the operators a session
--   knows, level by level (the picture of the chapter, in text).
-- トレーニングの解答：セッションが知っている演算子の表を、段ごとに
-- （章の絵を、文字で）。
module Lam.Answers (levelTable) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Syntax

-- 🪜 One line for each level, from 9 down to 0. Each operator is
--    shown with its leaning: <+> (l), : (r), == (n).
-- 段ごとに1行、9 から 0 まで。演算子は寄る向きと一緒に見せる
levelTable :: Operators -> [Text]
levelTable ops =
  [ T.pack (show n) <> "  " <> T.unwords (map shown here)
  | n <- [9, 8 .. 0]
  , let here = [ (o, f) | (o, f) <- Map.toList ops, fixityLevel f == n ]
  , not (null here) ]
  where
    shown (o, f) = o <> " (" <> lean (fixityAssoc f) <> ")"
    lean a = case a of
      LeftA  -> "l"
      RightA -> "r"
      NoneA  -> "n"
-- ANCHOR_END: all
