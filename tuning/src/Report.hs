-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- 📊 The guild's battle report: who fought, and who did the most damage.
-- ギルドのバトルの報告：誰が戦い、誰がいちばんダメージを与えたか
module Report
  ( Hit (..)
  , battleLog
  , heroesSlow
  , heroesFast
  , totalsSlow
  , totalsFast
  , topThree
  , Monster (..)
  , MonsterFast (..)
  , monsters
  , monstersFast
  ) where

import Data.Aeson   (ToJSON (..), defaultOptions, genericToEncoding)
import Data.List    (nub, sortOn)
import Data.Ord     (Down (..))
import Data.Text    (Text)
import Data.Text    qualified as T
import Data.Set     qualified as Set
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)

-- ANCHOR: log
-- ⚔️ One hit in the battle log.
-- バトルログの、攻撃1回
data Hit = Hit
  { hero   :: Text
  , damage :: Int
  }
  deriving (Show)

-- 📜 A made-up log: 20,000 hits by 2,000 heroes.
-- 作り物のログ：2,000人の勇者による、20,000回の攻撃
battleLog :: [Hit]
battleLog =
  [ Hit { hero = "hero-" <> T.pack (show (i `mod` 2000))
        , damage = (i * 37) `mod` 100 }
  | i <- [1 .. 20000 :: Int] ]
-- ANCHOR_END: log

-- ANCHOR: slow
-- 🐢 First drafts: easy to write, but they look through lists again
--    and again.
-- 下書き：書きやすいが、リストを何度も見直す
heroesSlow :: [Hit] -> [Text]
heroesSlow hits = nub (map (.hero) hits)

totalsSlow :: [Hit] -> [(Text, Int)]
totalsSlow hits = foldl' add [] hits
  where
    add totals h = case lookup h.hero totals of
      Nothing -> totals ++ [(h.hero, h.damage)]
      Just t  -> [ if n == h.hero then (n, t + h.damage) else (n, x)
                 | (n, x) <- totals ]
-- ANCHOR_END: slow

-- ANCHOR: fast
-- 🐇 The same, with a Set and a Map.
-- 同じものを、Set と Map で
heroesFast :: [Hit] -> [Text]
heroesFast hits = Set.toList (Set.fromList (map (.hero) hits))

totalsFast :: [Hit] -> [(Text, Int)]
totalsFast hits =
  Map.toList (Map.fromListWith (+) [ (h.hero, h.damage) | h <- hits ])
-- ANCHOR_END: fast

-- 🏆 The three heroes with the most damage.
-- ダメージの合計が大きい3人
topThree :: [(Text, Int)] -> [(Text, Int)]
topThree totals = take 3 (sortOn (\(n, t) -> (Down t, n)) totals)

-- ANCHOR: json
-- 🐉 A monster, turned into JSON with Generic (toJSON only).
-- モンスター（Generic で JSON に。toJSON だけ）
data Monster = Monster
  { name  :: Text
  , level :: Int
  , hp    :: Int
  }
  deriving (Generic, ToJSON)

-- 🐉 The same, with one more line: toEncoding.
-- 同じものに、1行足す：toEncoding
data MonsterFast = MonsterFast
  { name  :: Text
  , level :: Int
  , hp    :: Int
  }
  deriving (Generic)

instance ToJSON MonsterFast where
  toEncoding = genericToEncoding defaultOptions
-- ANCHOR_END: json

-- 🐉 10,000 monsters, in both forms.
-- 10,000体のモンスター（2つの形で）
monsters :: [Monster]
monsters =
  [ Monster ("slime-" <> T.pack (show i)) (i `mod` 50) (i * 3)
  | i <- [1 .. 10000 :: Int] ]

monstersFast :: [MonsterFast]
monstersFast =
  [ MonsterFast ("slime-" <> T.pack (show i)) (i `mod` 50) (i * 3)
  | i <- [1 .. 10000 :: Int] ]
-- ANCHOR_END: all
