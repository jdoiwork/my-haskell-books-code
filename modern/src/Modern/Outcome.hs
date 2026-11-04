-- ANCHOR: all
-- | The outcome of a quest, as an algebraic data type.
-- 依頼の結果を、代数的データ型で
module Modern.Outcome
  ( OutcomeRecord (..)
  , strange
  , Outcome (..)
  , describe
  , reward
  ) where

-- ANCHOR: record
-- 🗂️ The "one record with optional fields" way.
-- 「省略できるフィールドを持つ、1つのレコード」のやり方
data OutcomeRecord = OutcomeRecord
  { status :: String
  , gold   :: Maybe Int
  , place  :: Maybe String
  }
  deriving Show

-- 🤔 The types allow this: cleared, but no gold and a place?
-- 型はこれを許してしまう：クリアしたのに、ゴールドがなくて場所がある？
strange :: OutcomeRecord
strange = OutcomeRecord "cleared" Nothing (Just "cave")
-- ANCHOR_END: record

-- ANCHOR: outcome
-- 🎯 Each case carries exactly what it needs.
-- それぞれの場合が、必要なものだけを持つ
data Outcome
  = Cleared Int
  | Failed String
  | Fled
  deriving (Show, Eq)

-- 📝 Say what happened.
-- 何が起きたかを言う
describe :: Outcome -> String
describe (Cleared amount) = "cleared! +" <> show amount <> " G"
describe (Failed where_)  = "failed at " <> where_
describe Fled             = "ran away"

-- ANCHOR: reward
-- 💰 The gold we get.
-- もらえるゴールド
reward :: Outcome -> Int
reward (Cleared amount) = amount
reward _                = 0
-- ANCHOR_END: reward
-- ANCHOR_END: outcome
-- ANCHOR_END: all
