-- ANCHOR: all
-- 🔬 The model-based property test: the database against the Map.
-- モデルベースのプロパティテスト：データベースを Map と比べる
module ModelSpec (spec, prop_model, prop_modelDraft) where

import Model
import RosterSpec (withRoster)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- ANCHOR: prop
-- ⚖️ Do the same operations on the database and on the model.
--    Their answers and the final rosters must be the same.
-- 同じ操作を、データベースとモデルの両方にする。
-- 返事も、最後の名簿も、同じでなければならない
prop_model :: [Op] -> Property
prop_model ops = ioProperty $ withRoster $ \conn -> do
  realAnswers <- mapM (stepReal conn) ops
  realRoster  <- snapshot conn
  let (modelAnswers, modelRoster) = runModel stepModel ops
  pure ((realAnswers, realRoster) === (modelAnswers, modelRoster))
-- ANCHOR_END: prop

-- ANCHOR: propDraft
-- 🐞 The same test, with the draft model.
-- 同じテストを、下書きのモデルで
prop_modelDraft :: [Op] -> Property
prop_modelDraft ops = ioProperty $ withRoster $ \conn -> do
  realAnswers <- mapM (stepReal conn) ops
  realRoster  <- snapshot conn
  let (modelAnswers, modelRoster) = runModel stepModelDraft ops
  pure ((realAnswers, realRoster) === (modelAnswers, modelRoster))
-- ANCHOR_END: propDraft

-- ANCHOR: spec
spec :: Spec
spec =
  describe "the roster" $ do
    -- 「どんな操作の列でも、Map のモデルと同じに動く」
    prop "behaves like the Map model" prop_model
-- ANCHOR_END: spec
-- ANCHOR_END: all
