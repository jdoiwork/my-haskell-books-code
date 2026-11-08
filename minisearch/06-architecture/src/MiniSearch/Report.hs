-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | Turn search results into the text the command prints.
-- 検索結果を、コマンドが表示する文字にする。
module MiniSearch.Report (formatResults) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

import MiniSearch.Tokenizer (Tokenizer (..))
import MiniSearch.Types

-- 🖨️ 1. Dragon Tamer (0.49)
--       [dragon] tamer speaks to a [dragon] with ...
formatResults :: Tokenizer -> Index -> Text -> [Hit] -> Text
formatResults _ _ query [] =
  -- 「"unicorn" に当たる文書はありません」
  "No documents match \"" <> query <> "\".\n"
formatResults tok index query hits =
  T.unlines (concat (zipWith formatHit [1 :: Int ..] hits))
  where
    formatHit rank hit =
      [ T.pack (printf "%d. %s (%.2f)" rank (T.unpack (hitTitle hit))
                                       (hitScore hit))
      , "   " <> excerptOf tok query (bodyOf (hitDoc hit))
      ]
    bodyOf d = maybe "" docBody (Map.lookup d (indexDocs index))
-- ANCHOR_END: all
