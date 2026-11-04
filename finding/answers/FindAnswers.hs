module FindAnswers where

import Data.List (find)
import qualified Data.Map.Strict as Map
import Party (party)

-- ANCHOR: firstVeteran
-- The first adventurer whose level is 10 or more
-- レベル10以上の、最初の冒険者
firstVeteran :: Maybe (String, Int)
firstVeteran = find (\(_, lv) -> lv >= 10) party
-- ANCHOR_END: firstVeteran

-- ANCHOR: stock
-- Items in the bag and how many we have
-- 袋の中の道具と、その数
inventory :: Map.Map String Int
inventory = Map.fromList [("potion", 3), ("ether", 1)]

-- How many of the item we have (0 if we have none)
-- その道具をいくつ持っているか（なければ 0）
stock :: String -> Int
stock item = Map.findWithDefault 0 item inventory
-- ANCHOR_END: stock
