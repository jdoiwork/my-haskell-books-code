module EffectsSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Modern.Effects
import ModernAnswers (rollsNeeded, runLoaded)

spec :: Spec
spec = describe "Modern.Effects" $ do
  -- ANCHOR: scriptedTest
  -- 出目を決めておけば、戦いの結果も決まる
  it "runs a battle with scripted rolls" $
    runScripted [15, 4, 3, 12, 6] battle
      `shouldBe` (["hit for 4", "missed", "hit for 6"], 10)
  -- ANCHOR_END: scriptedTest
  -- 下書きは、台本しだいで 18 を超える（失敗するのが正解）
  it "the draft can go over 18 (expected to fail)" $ expectFailure $
    property $ \rolls ->
      let (_, total) = runScriptedDraft rolls battle
      in total >= 0 && total <= 18
  -- ANCHOR: rangeProp
  -- どんな台本でも、ダメージの合計は 0 から 18
  it "damage stays between 0 and 18" $ property $ \rolls ->
    let (_, total) = runScripted rolls battle
    in total >= 0 && total <= 18
  -- ANCHOR_END: rangeProp
  -- いつも最大の出目
  it "runLoaded" $
    runLoaded battle
      `shouldBe` (["hit for 6", "hit for 6", "hit for 6"], 18)
  -- 最大の出目なら、サイコロは6回
  it "rollsNeeded" $
    rollsNeeded battle `shouldBe` 6
