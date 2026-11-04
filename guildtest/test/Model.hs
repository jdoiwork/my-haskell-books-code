-- ANCHOR: all
-- ANCHOR: module
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- 🗺️ A simple model of the roster: just a Map.
-- 名簿の単純なモデル：ただの Map
module Model
  ( Op (..)
  , Answer (..)
  , Model
  , stepModel
  , stepModelDraft
  , runModel
  , stepReal
  , snapshot
  ) where

-- The roster, and SQLite.
-- 名簿と SQLite
import Roster
import Database.SQLite.Simple

-- Maps, Text, and catching errors.
-- Map、テキスト、エラーをつかまえる
import Data.Map.Strict   (Map)
import Data.Map.Strict   qualified as Map
import Data.Text         (Text)
import Control.Exception (try)

-- Random operations.
-- でたらめな操作
import Test.QuickCheck
-- ANCHOR_END: module

-- ANCHOR: op
-- 🎲 One operation on the roster.
-- 名簿への操作1つ
data Op
  = Add NewAdventurer
  | LevelUp Text
  | Remove Text
  deriving (Show)

-- 🔙 What an operation answers.
-- 操作の返事
data Answer
  = Done          -- added / 登録できた
  | Rejected      -- not added / 登録できなかった
  | Changed Int   -- rows changed / 変わった行の数
  deriving (Show, Eq)
-- ANCHOR_END: op

-- ANCHOR: arbitrary
-- 🎰 Make random operations. Only three names, so they often collide.
-- でたらめな操作を作る。名前は3つだけなので、よくぶつかる
instance Arbitrary Op where
  arbitrary = oneof
    [ Add     <$> genNew
    , LevelUp <$> genName
    , Remove  <$> genName
    ]

genName :: Gen Text
genName = elements ["Lambda", "Python", "Rust"]

genNew :: Gen NewAdventurer
genNew = do
  who <- genName
  job <- elements ["Mage", "Archer", "Knight"]
  lv  <- chooseInt (1, 10)
  pure (NewAdventurer { newName = who, newJob = job, newLevel = lv })
-- ANCHOR_END: arbitrary

-- ANCHOR: model
-- 📒 The model: name -> (job, level).
-- モデル：名前 → (職業, レベル)
type Model = Map Text (Text, Int)

-- 🧠 What the roster should do, written with a Map.
-- 名簿がするはずのことを、Map で書く
stepModel :: Model -> Op -> (Answer, Model)
stepModel model (Add new)
  | Map.member new.newName model = (Rejected, model)
  | otherwise                    = (Done, Map.insert new.newName entry model)
  where
    entry = (new.newJob, new.newLevel)
stepModel model (LevelUp who)
  | Map.member who model = (Changed 1, Map.adjust up who model)
  | otherwise            = (Changed 0, model)
  where
    up (job, lv) = (job, lv + 1)
stepModel model (Remove who)
  | Map.member who model = (Changed 1, Map.delete who model)
  | otherwise            = (Changed 0, model)
-- ANCHOR_END: model

-- ANCHOR: modelDraft
-- 🐞 A first draft: it forgets that names must be unique.
-- 下書き：名前がかぶってはいけないことを忘れている
stepModelDraft :: Model -> Op -> (Answer, Model)
stepModelDraft model (Add new) =
  (Done, Map.insert new.newName (new.newJob, new.newLevel) model)
stepModelDraft model op = stepModel model op
-- ANCHOR_END: modelDraft

-- ANCHOR: runModel
-- 🔁 Run the operations on the model, from an empty roster.
-- 空っぽの名簿から、モデルで操作を順に動かす
runModel :: (Model -> Op -> (Answer, Model)) -> [Op] -> ([Answer], Model)
runModel step ops = go Map.empty ops
  where
    go model []         = ([], model)
    go model (op : rest) =
      let (answer, next)    = step model op
          (answers, final) = go next rest
      in  (answer : answers, final)
-- ANCHOR_END: runModel

-- ANCHOR: real
-- 🗄️ Do an operation on the real database.
-- 本物のデータベースで、操作をする
stepReal :: Connection -> Op -> IO Answer
stepReal conn (Add new) = do
  result <- try (addAdventurer conn new)
  case result of
    Left (_ :: SQLError) -> pure Rejected
    Right ()             -> pure Done
stepReal conn (LevelUp who) = Changed <$> levelUp conn who
stepReal conn (Remove who)  = Changed <$> removeAdventurer conn who

-- 📸 Read the whole roster, in the same shape as the model.
-- 名簿の全体を、モデルと同じ形で読む
snapshot :: Connection -> IO Model
snapshot conn = do
  everyone <- listAdventurers conn
  pure (Map.fromList [ (a.name, (a.job, a.level)) | a <- everyone ])
-- ANCHOR_END: real
-- ANCHOR_END: all
