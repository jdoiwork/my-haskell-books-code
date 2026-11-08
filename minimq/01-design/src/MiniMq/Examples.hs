-- ANCHOR: all
-- | A small post office and some letters, to try in GHCi.
-- GHCi で試すための、小さな伝令所と手紙
module MiniMq.Examples
  ( surely
  , guildPost
  , morningLetters
  , trainingLetters
  ) where

-- ANCHOR: imports
import Data.Function ((&))

import MiniMq.Broker
import MiniMq.Types
-- ANCHOR_END: imports

-- ANCHOR: post
-- 🏤 The guild's post office: one sorting desk, two mailboxes.
--    Read it from top to bottom, like a pipe: `&` hands the broker on,
--    and `>>=` does the same for the steps that may fail.
-- ギルドの伝令所：仕分け台が1つ、郵便受けが2つ。
-- パイプのように上から読む：& がブローカーを次へ渡し、
-- 失敗するかもしれない手順では、>>= が同じことをする
guildPost :: Either BrokerError Broker
guildPost =
  emptyBroker
    & declareExchange "quests"
    & declareQueue "dragon-team"
    & declareQueue "herb-team"
    & bind "quests" "dragon" "dragon-team"
    >>= bind "quests" "herb" "herb-team"
-- ANCHOR_END: post

-- ANCHOR: letters
-- ✉️ The letters of the morning.
-- 朝に届いた手紙
morningLetters :: [Message]
morningLetters =
  [ Message "dragon" "A red dragon at the north gate"
  , Message "herb"   "Ten moon herbs, please"
  , Message "goblin" "Goblins in the cellar"
  , Message "dragon" "A baby dragon is lost"
  ]
-- ANCHOR_END: letters

-- 🏋️ For the training. / トレーニング用
trainingLetters :: [Message]
trainingLetters =
  [ Message "herb"   "Mint"
  , Message "dragon" "Blue dragon"
  , Message "herb"   "Sage"
  ]
-- 🙆 For GHCi: take the result out of steps we know cannot fail.
-- GHCi 用：失敗しないとわかっている手順から、結果を取り出す
surely :: Show e => Either e a -> a
surely = either (error . show) id
-- ANCHOR_END: all
