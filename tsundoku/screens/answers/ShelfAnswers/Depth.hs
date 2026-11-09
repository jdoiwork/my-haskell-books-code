-- ANCHOR: all
-- ANCHOR: head
-- | Training 2: a table two levels deep at most.
-- トレーニング2：目次の深さを2段までにそろえる
module ShelfAnswers.Depth (twoLevels) where

import Data.Text (Text)
import Data.Tree (Forest, Tree (..))
-- ANCHOR_END: head

-- ANCHOR: depth
-- ✂️ Keep the first two levels. Whatever is deeper goes into its
--    heading on the second level: the pages add up, the headings go.
-- 上の2段だけを残す。それより深いものは、2段目の見出しにまとめる：
-- ページ数は足し、見出しは消える
twoLevels :: Monoid u => Forest (Text, u) -> Forest (Text, u)
twoLevels = map (\(Node top kids) -> Node top (map fold kids))
  where
    fold (Node (heading, own) deeper) =
      Node (heading, own <> foldMap (foldMap snd) deeper) []
-- ANCHOR_END: depth
-- ANCHOR_END: all
