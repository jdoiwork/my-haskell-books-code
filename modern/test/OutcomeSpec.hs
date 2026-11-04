module OutcomeSpec (spec) where

import Test.Hspec

import Modern.Outcome (Outcome (..), reward)
import qualified Modern.Outcome as O
import ModernAnswers (Door (..), tryOpen)

spec :: Spec
spec = describe "Modern.Outcome" $ do
  -- 3つの場合を言える
  it "describes each outcome" $ do
    O.describe (Cleared 50) `shouldBe` "cleared! +50 G"
    O.describe (Failed "cave") `shouldBe` "failed at cave"
    O.describe Fled `shouldBe` "ran away"
  -- ゴールドがもらえるのはクリアしたときだけ
  it "reward" $
    map reward [Cleared 50, Failed "cave", Fled] `shouldBe` [50, 0, 0]
  -- 合う鍵だけが開ける
  it "tryOpen" $ do
    tryOpen "silver" (Locked "silver") `shouldBe` Open
    tryOpen "gold" (Locked "silver") `shouldBe` Locked "silver"
    tryOpen "gold" Closed `shouldBe` Open
