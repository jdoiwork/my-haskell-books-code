{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Turn search results into the text the command prints.
-- 検索結果を、コマンドが表示する文字にする。
module MiniSearch.Report (formatResults) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

import MiniSearch.Snippet (snippet)
import MiniSearch.Tokenize (tokenize)
import MiniSearch.Types

-- 🖨️ 1. Dragon Tamer (0.49)
--       [dragon] tamer speaks to a [dragon] with ...
formatResults :: Index -> Text -> [Hit] -> Text
formatResults _ query [] =
  -- 「"unicorn" に当たる文書はありません」
  "No documents match \"" <> query <> "\".\n"
formatResults index query hits =
  T.unlines (concat (zipWith formatHit [1 :: Int ..] hits))
  where
    tokens = tokenize query
    formatHit rank hit =
      [ T.pack (printf "%d. %s (%.2f)" rank (T.unpack (hitTitle hit))
                                       (hitScore hit))
      , "   " <> snippet 5 tokens (bodyOf (hitDoc hit))
      ]
    bodyOf d = maybe "" docBody (Map.lookup d (indexDocs index))
-- ANCHOR_END: all
