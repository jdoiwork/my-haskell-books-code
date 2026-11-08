-- ANCHOR: pragmas
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR_END: pragmas
-- ANCHOR: all
-- | Strings: many lines with """, and literals that are Text.
-- 文字列：""" で何行も。そして Text になるリテラル
module Strings (poem, banner) where

-- ANCHOR: imports
import Data.Text (Text)
-- ANCHOR_END: imports

-- ANCHOR: multiline
-- 📜 The common indentation is removed, and so is the first newline.
-- 共通の字下げは取りのぞかれ、最初の改行も取りのぞかれる
poem :: String
poem = """
    Roses are red,
      violets are blue.
    """

-- 🪧 With OverloadedStrings, the same literal can be a Text.
-- OverloadedStrings があれば、同じリテラルが Text にもなれる
banner :: Text
banner = """
  Welcome to the guild!
  """
-- ANCHOR_END: multiline
-- ANCHOR_END: all
