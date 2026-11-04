-- ANCHOR: all
-- ANCHOR: pragma
-- Let us write the parts of a parser with do.
-- パーサーの部品を do で並べられるようにする
{-# LANGUAGE ApplicativeDo #-}
-- ANCHOR_END: pragma

-- ANCHOR: module
-- 📋 The same list options as monlistRequestParser, written with do.
-- monlistRequestParser と同じ一覧のオプションを、do で書いた版
module Mondex.DoStyle
  ( monlistRequestParserDo
  ) where

import Options.Applicative

import Mondex.Command
  (MonlistRequest (..), MonsterOrder (..), elementOption, sortReader)
-- ANCHOR_END: module

-- ANCHOR: monlistRequestParserDo
-- 📋 [--element E] [--sort name|hp] [--json], with do.
-- [--element E] [--sort name|hp] [--json] を do で
monlistRequestParserDo :: Parser MonlistRequest
monlistRequestParserDo = do
  e    <- optional (elementOption "Show only this element")
  key  <- option sortReader sortSettings
  json <- switch jsonSettings
  pure (MonlistRequest e key json)
  where
    sortSettings = mconcat
      [ long "sort"
      , metavar "KEY"
      , value ByName
      , help "Sort by KEY: name or hp (default: name)"
      ]
    jsonSettings = mconcat
      [ long "json"
      , help "Print the monsters as JSON"
      ]
-- ANCHOR_END: monlistRequestParserDo

-- ANCHOR_END: all
