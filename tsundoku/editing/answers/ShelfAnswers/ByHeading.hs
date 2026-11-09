-- ANCHOR: all
-- | Training: the other way (A). The screen sends no numbers; the
--   server finds each line's number by its path of headings. It works
--   until a heading is renamed.
-- トレーニング：もう1つのやり方（A）。画面は番号を送らず、サーバーが
-- 見出しの道順で、行ごとの番号を探す。見出しの名前を直すまでは、うまくいく
module ShelfAnswers.ByHeading (byHeading) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Tree (Forest, Tree (..))

import Tsundoku.Types

-- 🔎 Give each line the number of the old entry with the same path of
--    headings ("第1部 > 第1章"), or none (a new line).
-- 行ごとに、見出しの道順（"第1部 > 第1章"）が同じ、前の項目の番号を付ける。
-- なければ番号なし（新しい行）
byHeading :: Toc u -> Forest (Text, Int) -> Forest Item
byHeading old = map (go [])
  where
    numbers = Map.fromList (paths [] old)
    paths above ts = concat
      [ (above ++ [heading e], entryId e) : paths (above ++ [heading e]) kids
      | Node e kids <- ts ]
    go above (Node (h, n) kids) =
      Node (Item (Map.lookup (above ++ [h]) numbers) h n)
           (map (go (above ++ [h])) kids)
-- ANCHOR_END: all
