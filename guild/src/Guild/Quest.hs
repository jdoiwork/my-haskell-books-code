-- ANCHOR: all
-- 📜 A quest on the guild's board.
-- ギルドの依頼
module Guild.Quest
  ( Rank (..)
  , Quest (..)
  , mkQuest
  ) where

import Data.Text (Text)

import Guild.Gold (Gold, mkGold)

-- 🎖️ Who may take the quest.
-- 依頼を受けられる冒険者のランク
data Rank = Novice | Veteran | Hero
  deriving (Show, Eq, Ord, Enum, Bounded)

-- 📜 A quest: what to do, for whom, and the reward.
-- 依頼：何をするか、誰向けか、報酬はいくらか
data Quest = Quest
  { title  :: Text
  , rank   :: Rank
  , reward :: Gold
  }
  deriving (Show, Eq)

-- 🛡️ Make a quest. A negative reward is rejected by mkGold.
-- 依頼を作る。マイナスの報酬は mkGold が断る
mkQuest :: Text -> Rank -> Int -> Maybe Quest
mkQuest t r amount =
  case mkGold amount of
    Just g  -> Just (Quest { title = t, rank = r, reward = g })
    Nothing -> Nothing
-- ANCHOR_END: all
