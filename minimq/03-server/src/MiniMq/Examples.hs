-- ANCHOR: all
-- | The guild's post office with three desks, to try in GHCi.
-- GHCi で試すための、仕分け台が3つある伝令所
module MiniMq.Examples
  ( surely
  , guildPost
  , letterTo
  , questKeys
  , trainingKeys
  ) where

import Data.Function ((&))

import MiniMq.Broker
import MiniMq.Types

-- ANCHOR: post
-- 🏤 Three desks, one for each way of handing out:
--    "notices" tells every team, "mail" goes to one adventurer,
--    and "quests" goes by the pattern of the key.
-- 配り方ごとに1つずつ、3つの仕分け台：
-- notices は全部の班へ、mail は冒険者1人へ、quests は札の模様で
guildPost :: Either BrokerError Broker
guildPost =
  emptyBroker
    & declareExchange "notices" Fanout
    & declareExchange "mail"    Direct
    & declareExchange "quests"  Topic
    & declareQueue "dragon-team"
    & declareQueue "north-guard"
    & declareQueue "archive"
    & bind "notices" ""                 "dragon-team"
    >>= bind "notices" ""               "north-guard"
    >>= bind "mail"    "dragon-team"    "dragon-team"
    >>= bind "quests"  "quest.dragon.*" "dragon-team"
    >>= bind "quests"  "quest.*.north"  "north-guard"
    >>= bind "quests"  "quest.#"        "archive"
-- ANCHOR_END: post

-- ✉️ A letter with a key and some text.
-- 札と本文の手紙
letterTo :: RoutingKey -> Message
letterTo key = Message key "..."

-- ANCHOR: keys
-- 🗝️ Keys of quests, to send to the "quests" desk.
-- quests の仕分け台へ送る、依頼の札
questKeys :: [RoutingKey]
questKeys =
  [ "quest.dragon.north"
  , "quest.dragon.south"
  , "quest.herb.north"
  , "quest.herb.east"
  , "quest"
  , "notice.dragon.north"
  ]
-- ANCHOR_END: keys

-- 🏋️ For the training. / トレーニング用
trainingKeys :: [RoutingKey]
trainingKeys =
  [ "quest.dragon"
  , "quest.dragon.north.cave"
  , "news.dragon.north"
  , "dragon"
  ]
-- 🙆 For GHCi: take the result out of steps we know cannot fail.
-- GHCi 用：失敗しないとわかっている手順から、結果を取り出す
surely :: Show e => Either e a -> a
surely = either (error . show) id
-- ANCHOR_END: all
