-- ANCHOR: all
-- ANCHOR: head
-- | Training, step 3: hits in a title first.
-- トレーニングのステップ3：題に当たったものを上に
module ShelfAnswers.TitlesFirst (titlesFirst) where

import Data.List (sortOn)

import Tsundoku.Search (Hit (..), Place (..))
-- ANCHOR_END: head

-- ANCHOR: titles
-- 🥇 The title first, then the creator, the headings, and the rest.
--    sortOn keeps the order of equal ones, so inside each group the
--    order the shelf gave stays.
-- 題を先に、それから作者、見出し、そのほか。sortOn は同じものの順番を
-- 変えないので、グループの中では、本棚がくれた順番のまま
titlesFirst :: [Hit] -> [Hit]
titlesFirst = sortOn (weight . place)
  where
    weight :: Place -> Int
    weight found = case found of
      InTitle     -> 0
      InCreator   -> 1
      InHeading _ -> 2
      InNote _ _  -> 3
      InText _    -> 3
-- ANCHOR_END: titles
-- ANCHOR_END: all
