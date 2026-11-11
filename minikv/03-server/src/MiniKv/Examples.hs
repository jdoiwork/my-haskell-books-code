-- ANCHOR: all
-- | Example command lists to try in GHCi.
-- GHCi で試すための、コマンドの列
module MiniKv.Examples
  ( goldCommands
  , partyCommands
  , skillCommands
  , profileCommands
  , mixUpCommands
  , trainingCommands
  ) where

import MiniKv.Types

-- ANCHOR: examples
-- 💰 Counting gold. / ゴールドを数える
goldCommands :: [Command]
goldCommands = [Incr "gold", Incr "gold", IncrBy "gold" 40]

-- 🧑‍🤝‍🧑 A list: the party, in order. / リスト：パーティーの並び
partyCommands :: [Command]
partyCommands =
  [ RPush "party" ["Lambda", "Python"]
  , LPush "party" ["Rust"]
  , LRange "party" 0 (-1)
  , LPop "party"
  , LLen "party"
  ]

-- ✨ A set: skills, without duplicates. / 集合：重なりのないスキル
skillCommands :: [Command]
skillCommands =
  [ SAdd "skills" ["fire", "ice", "fire"]
  , SIsMember "skills" "ice"
  , SMembers "skills"
  ]

-- 📇 A hash: fields of one adventurer. / ハッシュ：冒険者1人のフィールド
profileCommands :: [Command]
profileCommands =
  [ HSet "lambda" "job" "mage"
  , HSet "lambda" "level" "12"
  , HGet "lambda" "job"
  , HGetAll "lambda"
  ]

-- 🙅 Mixing up the kinds. / 種類の取りちがえ
mixUpCommands :: [Command]
mixUpCommands =
  [ Set "hero" "Lambda"
  , LPush "hero" ["x"]
  , Incr "hero"
  , TypeOf "hero"
  ]

-- 🏋️ The commands of the training, step 1. / トレーニングのステップ1
trainingCommands :: [Command]
trainingCommands =
  [ Set "a" "1", Incr "a", RPush "l" ["x", "y"], RPop "l", RPop "l"
  , RPop "l", Exists "l", Del ["a", "l"] ]
-- ANCHOR_END: examples
-- ANCHOR_END: all
