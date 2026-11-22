-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | The answers of the training of "Run it as a one-liner: like Perl".
--   The answers are Lam scripts (examples/oneliner/answer-*); this is
--   the Haskell they are compared with.
-- 「ワンライナーで動かす：Perl のように」のトレーニングの解答。解答は
-- Lam のスクリプト（examples/oneliner/answer-*）で、これは、それと
-- くらべる Haskell
module Lam.Answers
  ( paragraphs
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- ANCHOR: paragraphs
-- 📄 Step 3: the text, cut at the empty lines (Perl's -00): one piece
--    for each paragraph, its lines joined with " ". Many empty lines in
--    a row are one cut, as in Perl. A line of spaces is empty, too.
-- ステップ3：空の行で切った、段落ごとのかけら（Perl の -00）。段落の
-- 中の行は " " でつなぐ。空の行が続いても、切れ目は1つ。空白だけの行も
-- 空の行
paragraphs :: Text -> [Text]
paragraphs text = go (T.lines text)
  where
    blank = T.null . T.strip
    go ls = case dropWhile blank ls of
      []  -> []
      ls' -> let (para, rest) = break blank ls'
             in T.intercalate " " para : go rest
-- ANCHOR_END: paragraphs
-- ANCHOR_END: all
